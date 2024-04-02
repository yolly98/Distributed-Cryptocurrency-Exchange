-module(uuid_generator).

-export([start/0]).

start() ->
    yes = global:register_name(uuid_generator, self()),
    loop().

loop() ->
    receive {uuid, Table, From} ->
        {ok, UUID} = main_server_mnesia:get_new_uuid(Table),
        From ! {uuid, UUID}
    end,
    loop().