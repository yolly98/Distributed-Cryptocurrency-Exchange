-module(coin_node_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 5},
    CowboyListener = #{
        id => listener,
        start => {cowboy_listener, start, []},
        restart => permanent,
        shutdown => brutal_kill
    },
    ChildSpecs = [CowboyListener],
    {ok, {SupFlags, ChildSpecs}}.
