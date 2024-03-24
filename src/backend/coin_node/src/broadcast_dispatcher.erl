-module(broadcast_dispatcher).

-export([start/0]).

start() ->
    global:register_name({dispatcher, node(), self()}, self()),
    loop().

loop() ->
    receive {update_market_value, Coin, MarketValue} ->
        RegisteredPids = global:registered_names(),
        lists:foreach(fun(RegisteredPid) ->
            case RegisteredPid of
                {ws, Node, Pid} ->
                    if
                        Node == node() ->
                            Msg = jsone:encode(#{<<"coin">> => list_to_binary(Coin), <<"market_value">> => MarketValue}),
                            Pid ! {broadcast, Msg};
                        Node =/= node() ->
                            ok
                    end;
                _ ->
                    ok
            end
        end, RegisteredPids)
    end,
    loop().