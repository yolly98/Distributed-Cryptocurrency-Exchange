-module(order_handler).

-export([
    init/2,
    content_types_accepted/2,
    content_types_provided/2,
    post_handler/2,
    allowed_methods/2
]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

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
                {atomic, ok} = mnesia:transaction(fun() -> coin_node_mnesia:sell(User, Coin, Quantity) end);
            "buy" ->
                {atomic, ok} = mnesia:transaction(fun() -> coin_node_mnesia:buy(User, Coin, Quantity) end)
        end,
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{<<"Operation">> => <<"Success">>}), Req1),
        {true, Req2, State}
    catch
        error:_ ->
            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsone:encode(#{<<"Operation">> => <<"Failed">>}), Req1),
            {halt, Req3, State}
    end.