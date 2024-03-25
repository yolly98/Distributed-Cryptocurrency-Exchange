-module(order_handler).

-export([
    init/2,
    content_types_accepted/2,
    content_types_provided/2,
    post_handler/2,
    allowed_methods/2
]).

init(Req, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, accept">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req2),
	{cowboy_rest, Req3, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
        {<<"application/json">>, post_handler}
    ], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, post_handler}
	], Req, State}.

post_handler(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"opcode">> := BinaryOpCode, <<"user">> := BinaryUser, <<"coin">> := BinaryCoin, <<"quantity">> := Quantity} = jsone:decode(Body),
    OpCode = binary_to_list(BinaryOpCode),
    User = binary_to_list(BinaryUser),
    Coin = binary_to_list(BinaryCoin),
    
    try
        case OpCode of
            "sell" ->
                {atomic, {Deposit, Asset, MarketValue}} = mnesia:transaction(fun() -> 
                    ok = coin_node_mnesia:sell(User, Coin, Quantity),
                    {ok, Deposit} = coin_node_mnesia:get_deposit(User),
                    {ok, Asset} = coin_node_mnesia:get_asset_by_user(User, Coin),
                    {ok, MarketValue} = coin_node_mnesia:get_coin_value(Coin),
                    {Deposit, Asset, MarketValue}
                end);
            "buy" ->
                {atomic, {Deposit, Asset, MarketValue}} = mnesia:transaction(fun() -> 
                    ok = coin_node_mnesia:buy(User, Coin, Quantity),
                    {ok, Deposit} = coin_node_mnesia:get_deposit(User),
                    {ok, Asset} = coin_node_mnesia:get_asset_by_user(User, Coin),
                    {ok, MarketValue} = coin_node_mnesia:get_coin_value(Coin),
                    {Deposit, Asset, MarketValue}
                end)
        end,
        RegisteredPids = global:registered_names(),
        lists:foreach(fun(RegisteredPid) ->
            case RegisteredPid of
                {dispatcher, _, Pid} ->
                    Pid ! {update_market_value, Coin, MarketValue};
                _ ->
                    ok
            end
        end, RegisteredPids),
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{<<"status">> => <<"success">>, <<"balance">> => Deposit, <<"asset">> => Asset}), Req1),
        {true, Req2, State}
    catch
        error:_ ->
            {atomic, {EffectiveDeposit, EffectiveAsset}} = mnesia:transaction(fun() -> 
                {ok, EffectiveDeposit} = coin_node_mnesia:get_deposit(User),
                {ok, EffectiveAsset} = coin_node_mnesia:get_asset_by_user(User, Coin),
                {EffectiveDeposit, EffectiveAsset}
            end),
            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsone:encode(#{<<"status">> => <<"failed">>, <<"balance">> => EffectiveDeposit, <<"asset">> => EffectiveAsset}), Req1),
            {halt, Req3, State}
    end.