-module(coin_handler).

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

prepare_coins(Coins) ->
    prepare_coins(Coins, []).

prepare_coins([], PreparedCoins) ->
    {ok, PreparedCoins};

prepare_coins([Coin | RemainingCoin], PreparedCoins) ->
    {coin, Name, MarketValue} = Coin,
    PreparedCoin = #{
        <<"coin">> => list_to_binary(Name),
        <<"market_value">> => MarketValue
    },
    prepare_coins(RemainingCoin, PreparedCoins ++ [PreparedCoin]).

get_handler(Req, State) ->
    try
        {atomic, Coins} = mnesia:transaction(fun() ->
            {ok, Coins} = coin_node_mnesia:get_coins(),
            Coins
        end),
        {ok, PreparedCoins} = prepare_coins(Coins),
        Reply = jsone:encode(#{<<"coins">> => PreparedCoins}),
        {Reply, Req, State}
    catch
        error:_ -> 
            cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsone:encode(#{<<"status">> => <<"failed">>}), Req),
            {halt, Req, State}
    end.