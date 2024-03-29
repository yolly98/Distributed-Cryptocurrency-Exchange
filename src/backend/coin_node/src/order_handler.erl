-module(order_handler).

-export([
    init/2,
    content_types_accepted/2,
    content_types_provided/2,
    request_dispatcher/2,
    get_handler/2,
    post_handler/2,
    delete_resource/2,
    allowed_methods/2
]).

init(Req, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, DELETE, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, accept">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req2),
	{cowboy_rest, Req3, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
        {<<"application/json">>, request_dispatcher}
    ], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, request_dispatcher}
	], Req, State}.

request_dispatcher(Req, State) -> 
    case cowboy_req:method(Req) of
        <<"GET">> ->
            get_handler(Req, State);
		_ ->
			post_handler(Req, State)
		end.

prepare_pending_orders(Orders) ->
    prepare_pending_orders(Orders, []).

prepare_pending_orders([], PendingOrders) ->
    {ok, PendingOrders};

prepare_pending_orders([Order | RemainingOrders], PendingOrders) ->
    {_, {_, Timestamp, _}, Type, _, Quantity} = Order,
    PendingOrder = #{
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"type">> => list_to_binary(Type),
        <<"quantity">> => Quantity
    },
    prepare_pending_orders(RemainingOrders, PendingOrders ++ [PendingOrder]).

get_handler(Req, State) ->
    #{user := BinaryUser, coin := BinaryCoin} = cowboy_req:match_qs([{user, nonempty}, {coin, nonempty}], Req),
    User = binary_to_list(BinaryUser),
    Coin = binary_to_list(BinaryCoin),
    {atomic, {ok, Orders}} = mnesia:transaction(fun() -> coin_node_mnesia:get_pending_orders(User, Coin) end),
    {ok, PendingOrders} = prepare_pending_orders(Orders),
    Reply = jsone:encode(#{<<"orders">> => PendingOrders}),
    {Reply, Req, State}.

post_handler(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"type">> := BinaryType, <<"user">> := BinaryUser, <<"coin">> := BinaryCoin, <<"quantity">> := Quantity} = jsone:decode(Body),
    Type = binary_to_list(BinaryType),
    User = binary_to_list(BinaryUser),
    Coin = binary_to_list(BinaryCoin),

%    try
        case Type of
            "sell" ->
                true = Quantity > 0, 
                {atomic, {Deposit, Asset, MarketValue, CompletedTransactions, NewPendingOrder}} = mnesia:transaction(fun() -> 
                    {ok, CompletedTransactions, NewPendingOrder} = coin_node_mnesia:sell(User, Coin, Quantity),
                    {ok, Deposit} = coin_node_mnesia:get_deposit(User),
                    {ok, Asset} = coin_node_mnesia:get_asset_by_user(User, Coin),
                    {ok, MarketValue} = coin_node_mnesia:get_coin_value(Coin),
                    {Deposit, Asset, MarketValue, CompletedTransactions, NewPendingOrder}
                end);
            "buy" ->
                true = Quantity > 0,
                {atomic, {Deposit, Asset, MarketValue, CompletedTransactions, NewPendingOrder}} = mnesia:transaction(fun() -> 
                    {ok, CompletedTransactions, NewPendingOrder} = coin_node_mnesia:buy(User, Coin, Quantity),
                    {ok, Deposit} = coin_node_mnesia:get_deposit(User),
                    {ok, Asset} = coin_node_mnesia:get_asset_by_user(User, Coin),
                    {ok, MarketValue} = coin_node_mnesia:get_coin_value(Coin),
                    {Deposit, Asset, MarketValue, CompletedTransactions, NewPendingOrder}
                end)
        end,
        RegisteredPids = global:registered_names(),
        lists:foreach(fun(RegisteredPid) ->
            case RegisteredPid of
                {dispatcher, _, Pid} ->
                    Pid ! {update_market_value, Coin, MarketValue, CompletedTransactions};
                _ ->
                    ok
            end
        end, RegisteredPids),
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{
            <<"status">> => <<"success">>,
            <<"balance">> => Deposit,
            <<"asset">> => Asset,
            <<"new_pending_order">> => NewPendingOrder
        }), Req1),
        {true, Req2, State}.
%    catch
%        error:_ ->
%            {atomic, {EffectiveDeposit, EffectiveAsset}} = mnesia:transaction(fun() -> 
%                {ok, EffectiveDeposit} = coin_node_mnesia:get_deposit(User),
%                {ok, EffectiveAsset} = coin_node_mnesia:get_asset_by_user(User, Coin),
%                {EffectiveDeposit, EffectiveAsset}
%            end),
%            Reply = jsone:encode(#{
%                <<"status">> => <<"failed">>, 
%                <<"balance">> => EffectiveDeposit, 
%                <<"asset">> => EffectiveAsset,
%                <<"quantity">> => Quantity
%            }),
%            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Reply, Req1),
%            {halt, Req3, State}
%    end.

delete_resource(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"user">> := BinaryUser, <<"timestamp">> := BinaryTimestamp, <<"coin">> := BinaryCoin} = jsone:decode(Body),
    User = binary_to_list(BinaryUser),
    Coin = binary_to_list(BinaryCoin),
    Timestamp = list_to_integer(binary_to_list(BinaryTimestamp)),

    try
        {atomic, ok} = mnesia:transaction(fun() -> 
            {ok, {_, _, Type, _, Quantity}} = coin_node_mnesia:delete_order({order_key, Timestamp, User}, Coin),
            if 
                Type == "sell" ->
                    ok = coin_node_mnesia:add_asset(User, Coin, Quantity);
                Type == "buy" ->
                    ok = coin_node_mnesia:add_deposit(User, Quantity)
            end
        end),
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{
            <<"status">> => <<"success">>
        }), Req1),
        {true, Req2, State}
    catch
        error:_ ->
            {false, Req1, State}
    end.