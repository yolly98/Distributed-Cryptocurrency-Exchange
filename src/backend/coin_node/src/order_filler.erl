-module(order_filler).

-export([start/1, loop/1]).

start(Coin) ->
    io:format("~p started \n", [Coin ++ "_order_filler"]),
    Name = list_to_atom(Coin ++ "_order_filler"),
    Pid = spawn_link(?MODULE, loop, [Coin]),
    global:register_name({Name, node()}, Pid),
    {ok, Pid}.

loop(Coin) ->
    receive fill_orders ->
        {atomic, {CompletedTransactions, NewMarketValue}} = mnesia:transaction(fun() -> 
            {ok, MarketValue} = coin_node_mnesia:get_coin_value(Coin),
            {ok, CompletedTransactions, NewMarketValue} = coin_node_mnesia:fill_orders(Coin, MarketValue),
            {CompletedTransactions, NewMarketValue}
        end),
        if
            CompletedTransactions == [] -> ok;
            CompletedTransactions =/= [] ->
                RegisteredPids = global:registered_names(),
                lists:foreach(fun(RegisteredPid) ->
                    case RegisteredPid of
                        {dispatcher, _, Pid} ->
                            Pid ! {update_market_value, Coin, NewMarketValue, CompletedTransactions};
                        _ ->
                            ok
                    end
                end, RegisteredPids)
        end
    end,
    loop(Coin).