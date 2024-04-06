-module(main_server_mnesia).

-record(asset_key, {user_id::string(), coin_id::string()}).
-record(asset, {asset_key, quantity::float()}).

-record(coin, {id::string(), value::float()}).

-record(user, {id::string(), password::term(), deposit::float()}).

-record(order, {uuid::integer(), timestamp::integer(), user_id::string(), type::string(), coin_id::string(), quantity::float(), limit::float()}).

-record(transaction, {uuid::integer(), timestamp::integer(), seller::string(), buyer::string(), coin_id::string(), coins::float(), market_value::float(), new_market_value::float()}).

-record(uuid, {table::string(), last_uuid::integer()}).

-export([
    create_database/0,
    add_node_to_schema/3,
    add_coin/3,
    get_new_uuid/1
]).

create_database() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(uuid, [
        {disc_copies, [node()]},
        {attributes, record_info(fields, uuid)}
    ]),

    mnesia:create_table(coin, [
        {type, ordered_set},
        {disc_copies, [node()]},
        {attributes, record_info(fields, coin)}
    ]),

    mnesia:create_table(user, [
        {disc_copies, [node()]},
        {attributes, record_info(fields, user)}
    ]),

    mnesia:create_table(asset, [
        {disc_copies, [node()]},
        {attributes, record_info(fields, asset)}
    ]),

    mnesia:create_table(transaction, [
        {type, ordered_set},
        {disc_copies, [node()]},
        {attributes, record_info(fields, transaction)}
    ]),
    
    ok = add_uuid("transaction"),
    ok = add_coin("btc", 60000, []),
    ok = add_coin("eth", 2500, []),
    ok = add_coin("ada", 0.5, []),
    ok = add_coin("sol", 162, []),
    ok = add_coin("xrp", 54, []),
    ok = add_coin("trx", 23, []),
    ok = add_coin("leo", 35, []),
    ok = add_coin("til", 455, []),
    ok = add_coin("cro", 344, []),
    ok = add_coin("bnb", 22, []),
    ok = add_coin("itm", 10, []),
    ok = add_coin("slm", 1, []),
    ok = add_coin("ptz", 4.003, []),
    ok = add_coin("lmb", 91.2345, []),
    ok = add_coin("mmf", 0.00001, []),
    ok = add_coin("pfc", 66.234, []),
    ok = add_coin("lor", 666, []),
    ok = add_coin("mrr", 5664.35353646, []).

create_table_copies(_, [], _) ->
    ok;

create_table_copies(Node , [Table | RemainingTables], CopiesType) ->
    {atomic, _} = mnesia:add_table_copy(Table, Node, CopiesType),
    create_table_copies(Node, RemainingTables, CopiesType).

add_node_to_schema(Node, RamTables, DiscTables) ->
    % add node to schema, tables can be accessed remotely
    {ok, _} = mnesia:change_config(extra_db_nodes, [Node]),
    % enable disc copies in the node
    {atomic, _} = mnesia:change_table_copy_type(schema, Node, disc_copies),
    % create ram copies of tables in the node
    create_table_copies(Node, RamTables, ram_copies),
    % create disc copies of tables in the node
    create_table_copies(Node, DiscTables, disc_copies).

add_coin(Coin, Value, Nodes) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        CoinRecord = #coin{id='$1', value='$2'},
        Guard = {'==', '$1', Coin},
        Coins = mnesia:select(coin, [{CoinRecord, [Guard], ['$_']}]),
        case Coins == [] of
            true ->
                ok = mnesia:write(#coin{id=Coin, value=Value});
            false ->
                error
        end
    end),
    {atomic, ok} = mnesia:create_table(list_to_atom(Coin ++ "_order"), [
        {type, ordered_set},
        {disc_copies, Nodes ++ [node()]},
        {attributes, record_info(fields, order)}
    ]),
    ok = add_uuid(Coin ++ "_order").

add_uuid(Table) ->
    {atomic, ok} = mnesia:transaction(fun() -> 
        UUIDRecord = #uuid{table='$1', last_uuid='$2'},
        Guard = {'==', '$1', Table},
        UUIDs = mnesia:select(uuid, [{UUIDRecord, [Guard], ['$_']}]),
        case UUIDs == [] of
            true ->
                ok = mnesia:write(#uuid{table=Table, last_uuid=0});
            false ->
                error
        end
    end),
    ok.

get_new_uuid(Table) ->
    {atomic, {ok, UUID}} = mnesia:transaction(fun() ->
        UUIDRecord = #uuid{table='$1', last_uuid='$2'},
        Guard = {'==', '$1', Table},
        UUIDs = mnesia:select(uuid, [{UUIDRecord, [Guard], ['$2']}]),
        case UUIDs == [] of
            true ->
                error;
            false ->
                [UUID | _] = UUIDs,
                NewUUID = UUID + 1,
                ok = mnesia:write(#uuid{table=Table, last_uuid=NewUUID}),
                {ok, UUID}
        end
    end),
    {ok, UUID}.