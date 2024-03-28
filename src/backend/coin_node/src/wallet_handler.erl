-module(wallet_handler).

-export([
    init/2,
    content_types_provided/2,
    get_handler/2
]).

init(Req, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, accept">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req2),
	{cowboy_rest, Req3, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, get_handler}
	], Req, State}.

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