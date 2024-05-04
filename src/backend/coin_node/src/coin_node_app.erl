-module(coin_node_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(node(), self()),
    mnesia:start(),
    
    {main_server, main_server@main_server} ! ok,

    timer:sleep(5000),
    io:format("coin node started\n"),
    coin_node_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    io:format("coin node stopped\n").
