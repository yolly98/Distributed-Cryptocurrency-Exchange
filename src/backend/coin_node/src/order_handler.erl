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
    {_, UUID, Timestamp, _, Type, _, Quantity, Limit} = Order,
    PendingOrder = #{
        <<"uuid">> => list_to_binary(integer_to_list(UUID)),
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"type">> => list_to_binary(Type),
        <<"quantity">> => Quantity,
        <<"limit">> => Limit
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
    #{<<"type">> := BinaryType, <<"user">> := BinaryUser, <<"coin">> := BinaryCoin, <<"quantity">> := Quantity, <<"limit">> := Limit} = jsone:decode(Body),
    Type = binary_to_list(BinaryType),
    User = binary_to_list(BinaryUser),
    Coin = binary_to_list(BinaryCoin),

    try
        true = Quantity > 0,
        {atomic, {UUID, Timestamp, Deposit, Asset}} = mnesia:transaction(fun() -> 
            {ok, UUID, Timestamp} = coin_node_mnesia:insert_new_order(User, Type, Coin, Quantity, Limit),

            if 
                Type == "sell" ->
                    {ok, {asset, _, AssetQuantity}} = coin_node_mnesia:get_asset(User, Coin),
                    if
                        AssetQuantity < Quantity -> 
                            throw("AssetQuantity lower than Quantity in order post handler");
                        AssetQuantity >= Quantity ->
                            ok = coin_node_mnesia:sub_asset(User, Coin, Quantity)
                    end;
                Type == "buy" ->
                    {ok, {user, _, _, Deposit}} = coin_node_mnesia:get_user(User),
                    if 
                        Deposit < Quantity -> 
                            throw("Deposit lower than Quantity in order post handler");
                        Deposit >= Quantity ->
                            ok = coin_node_mnesia:sub_deposit(User, Quantity)
                    end
            end,
            {ok, NewDeposit} = coin_node_mnesia:get_deposit(User),
            {ok, NewAsset} = coin_node_mnesia:get_asset_by_user(User, Coin),
            {UUID, Timestamp, NewDeposit, NewAsset}
        end),
        NewPendingOrder = #{
            <<"uuid">> => list_to_binary(integer_to_list(UUID)),
            <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)), 
            <<"quantity">> => Quantity,
            <<"limit">> => Limit
        },
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{
            <<"status">> => <<"success">>,
            <<"balance">> => Deposit,
            <<"asset">> => Asset,
            <<"new_pending_order">> => NewPendingOrder
        }), Req1),

        % send fill_orders message to order_filler process
        global:whereis_name({list_to_atom(Coin ++ "_order_filler"), node()}) ! fill_orders,
        
        {true, Req2, State}
    catch
        error:_ ->
            Reply = jsone:encode(#{<<"status">> => <<"failed">>}),
            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Reply, Req1),
            {halt, Req3, State}
    end.

delete_resource(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"uuid">> := BinaryUUID, <<"coin">> := BinaryCoin} = jsone:decode(Body),
    UUID = list_to_integer(binary_to_list(BinaryUUID)),
    Coin = binary_to_list(BinaryCoin),

    try
        {atomic, ok} = mnesia:transaction(fun() -> 
            {ok, {_, _, _, User, Type, _, Quantity, _}} = coin_node_mnesia:delete_order(UUID, Coin),
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