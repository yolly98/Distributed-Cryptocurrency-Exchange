-module(main_server_app).

-behaviour(application).

-export([start/2, stop/1]).

connect_nodes([]) ->
    ok;

connect_nodes([Node | RemainingNodes]) ->
    true = net_kernel:connect_node(Node),
    spawn(Node, application, start, [coin_node]),
    receive ok ->
        Tables = lists:delete(uuid, lists:delete(schema, mnesia:system_info(tables))),
        main_server_mnesia:add_node_to_schema(Node, [], Tables)
    end,
    io:format("Spawned node ~p\n", [Node]),
    connect_nodes(RemainingNodes).

start(_StartType, _StartArgs) ->
    io:format("main server started\n"),
    true = register(main_server, self()),

    {ok, Nodes} = application:get_env(nodes),
    if 
        Nodes == [] -> throw("No nodes configurated\n");
        Nodes =/= [] -> ok
    end,

    mnesia:start(node()),
    main_server_mnesia:create_database(),
    ok = connect_nodes(Nodes),
    io:format("nodes connection completed\n"),

    main_server_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    io:format("main server stopped\n").
