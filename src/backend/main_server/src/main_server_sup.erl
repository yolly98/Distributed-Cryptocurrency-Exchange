-module(main_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    UUIDGenerator = #{
        id => uuid_generator,
        start => {uuid_generator, start, []},
        restart => permanent,
        shutdown => brutal_kill
    },
    ChildSpecs = [UUIDGenerator],
    {ok, {SupFlags, ChildSpecs}}.