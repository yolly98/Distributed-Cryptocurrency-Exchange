-module(cowboy_listener).

-export([start/0, kill/0]).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/res", rest_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }).

kill() ->
    ok = cowboy:stop_listener(http).