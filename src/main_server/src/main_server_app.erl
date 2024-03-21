-module(main_server_app).

-behaviour(application).

-export([start/2, stop/1]).

connect_nodes([]) ->
    ok;

connect_nodes([Node | RemainingNodes]) ->
    true = net_kernel:connect_node(Node),
    connect_nodes(RemainingNodes),
    spawn_monitor(Node, application, start, [coin_node]),
    io:format("Spawned node ~p\n", [Node]).

start(_StartType, _StartArgs) ->
    io:format("exchange prototype started\n"),
    
    {ok, Nodes} = application:get_env(nodes),
    if 
        Nodes == [] -> throw("No nodes configurated\n");
        Nodes =/= [] -> ok
    end,
    ok = connect_nodes(Nodes),
    
    io:format("nodes connection completed\n"),

    mnesia:start(Nodes ++ node()),
    main_server_mnesia:create_database(),
    
    true = register(main_server, self()),

    io:format("process spawns completed\n"),

    main_server_sup:start_link().

    % handle monitored process death

stop(_State) ->
    mnesia:stop(),
    io:format("exchange prototype stopped\n").
