-module(transaction_handler).

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

prepare_transactions(Transactions) ->
    prepare_transactions(Transactions, []).

prepare_transactions([], PreparedTransactions) ->
    {ok, PreparedTransactions};

prepare_transactions([Transaction | RemainingTransactions], PreparedTransactions) ->
    {_, _, Timestamp, _, _, _, Quantity, MarketValue, NewMarketValue} = Transaction,
    PreparedTransaction = #{
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"quantity">> => Quantity,
        <<"market_value">> => MarketValue,
        <<"new_market_value">> => NewMarketValue
    },
    prepare_transactions(RemainingTransactions, PreparedTransactions ++ [PreparedTransaction]).

get_handler(Req, State) ->
    #{coin := BinaryCoin, seconds := BinarySeconds} = cowboy_req:match_qs([{coin, nonempty}, {seconds, nonempty}], Req),
    Coin = binary_to_list(BinaryCoin),
    Seconds = binary_to_integer(BinarySeconds),
    {atomic, {ok, Transactions}} = mnesia:transaction(fun() -> coin_node_mnesia:get_transactions_history(Coin, Seconds) end),
    {ok, PreparedTransactions} = prepare_transactions(Transactions),
    Reply = jsone:encode(#{ <<"transactions">> => PreparedTransactions }),
    {Reply, Req, State}.
