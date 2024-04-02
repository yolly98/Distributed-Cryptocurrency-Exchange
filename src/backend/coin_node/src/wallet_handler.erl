-module(wallet_handler).

-export([
    init/2,
    content_types_accepted/2,
    content_types_provided/2,
    request_dispatcher/2,
    get_handler/2,
    allowed_methods/2
]).

init(Req, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, PUT, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, accept">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req2),
	{cowboy_rest, Req3, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

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
			put_handler(Req, State)
		end.

get_handler(Req, State) ->
    #{user := BinaryUser, coin := BinaryCoin, balance := BinaryBalance} = cowboy_req:match_qs([{user, nonempty}, {coin, nonempty}, {balance, nonempty}], Req),
    User = binary_to_list(BinaryUser),
    Coin = binary_to_list(BinaryCoin),
    Balance = binary_to_list(BinaryBalance),
    {atomic, Reply} = mnesia:transaction(fun() ->
        if
            Coin == "all" ->
                {ok, Assets} = coin_node_mnesia:get_assets_by_user(User);
            Coin =/= "all" ->
                {ok, Assets} = coin_node_mnesia:get_asset_by_user(User, Coin)
        end,
        if 
            Balance == "true" ->
                case coin_node_mnesia:get_deposit(User) of 
                    {ok, Deposit} -> 
                        Reply = jsone:encode(#{<<"assets">> => Assets, <<"balance">> => Deposit});
                    error -> 
                        Reply = error
                end;
            Balance =/= "true" ->
                Reply = jsone:encode(#{<<"assets">> => Assets})
        end,
        Reply
    end),
    case Reply of
        error -> 
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, jsone:encode(#{<<"status">> => <<"not found">>}), Req),
            {halt, Req, State};
        _ ->
            {Reply, Req, State}
    end.

put_handler(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"user">> := BinaryUser, <<"type">> := BinaryType, <<"operation">> := BinaryOperation, <<"quantity">> := Quantity} = jsone:decode(Body), 
    User = binary_to_list(BinaryUser),
    Type = binary_to_list(BinaryType),
    Operation = binary_to_list(BinaryOperation),

    try
        case Type of
            "deposit" ->
                case Operation of
                    "add" ->
                        true = Quantity > 0, 
                        {atomic, ok} = mnesia:transaction(fun() ->
                            ok = coin_node_mnesia:add_deposit(User, Quantity)
                        end);
                    "sub" ->
                        true = Quantity > 0, 
                        {atomic, ok} = mnesia:transaction(fun() ->
                            ok = coin_node_mnesia:sub_deposit(User, Quantity)
                        end)
                end;
            _ ->
                case Operation of
                    "add" ->
                        true = Quantity > 0, 
                        {atomic, ok} = mnesia:transaction(fun() ->
                            ok = coin_node_mnesia:add_asset(User, Type, Quantity)
                        end);
                    "sub" ->
                        true = Quantity > 0, 
                        {atomic, ok} = mnesia:transaction(fun() ->
                            ok = coin_node_mnesia:sub_asset(User, Type, Quantity)
                        end)
                end
        end,
        Req2 = cowboy_req:set_resp_body(jsone:encode(#{<<"status">> => <<"success">>}), Req1),
        {true, Req2, State}
    catch
        error:_ ->
            Reply = jsone:encode(#{<<"status">> => <<"failed">>}),
            Req3 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Reply, Req1),
            {halt, Req3, State}
    end.

