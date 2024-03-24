-module(cowboy_listener).

-export([start/0, kill/0]).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/order", order_handler, []},
            {"/api/wallet", wallet_handler, []},
            {"/websocket", websocket_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }).

kill() ->
    cowboy:stop_listener(listener),
    ok.