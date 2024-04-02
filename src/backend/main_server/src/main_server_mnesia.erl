-module(main_server_mnesia).

-record(asset_key, {user_id::string(), coin_id::string()}).
-record(asset, {asset_key, quantity::float()}).

-record(coin, {id::string(), value::float()}).

-record(user, {id::string(), deposit::float()}).

-record(order, {uuid::integer(), timestamp::integer(), user_id::string(), type::string(), coin_id::string(), quantity::float()}).

-record(transaction, {uuid::integer(), timestamp::integer(), seller::string(), buyer::string(), coin_id::string(), coins::float(), market_value::float(), new_market_value::float()}).

-record(uuid, {table::string(), last_uuid::integer()}).

-export([
    create_database/0,
    add_node_to_schema/3,
    add_coin/3,
    get_new_uuid/1
%    remove_coin/1
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
    ok = add_coin("btc1", 20, []),
    ok = add_coin("btc2", 5, []),
    ok = add_coin("btc3", 0.5, []),

    % test
    {atomic, _} = mnesia:transaction(fun() ->
        insert_new_user("Stefano", 100000),
        insert_new_user("Andrea", 100000),
        insert_new_asset("Andrea", "btc1", 100000),
        insert_new_asset("Stefano", "btc1", 100000)
    end).

create_table_copies(Node, [], CopiesType) ->
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

% rollback_orders([]) ->
%     ok;
% 
% rollback_orders([{_, OrderKey, Type, CoinId, Quantity} | RemainingOrders]) ->
%     if 
%         Type == "sell" ->
%             add_asset(OrderKey#order_key.user_id, CoinId, Quantity);
%         Type == "buy" ->
%             add_deposit(OrderKey#order_key.user_id, Quantity)
%     end,
%     rollback_orders(RemainingOrders).
% 
% 
% add_asset(UserId, CoinId, Quantity) ->
%     AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
%     Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
%     Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
%     case Assets == [] of
%         true -> 
%             ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=Quantity});
%         false ->
%             [Asset | _] = Assets,
%             NewQuantity = Asset#asset.quantity + Quantity,
%             ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=NewQuantity})
%     end.
%
% remove_coin(Coin) ->
%     mnesia:transaction(fun() ->
%         Table = list_to_atom(Coin ++ "_order"),
%         OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
%         Guards = [{'==', '$4', Coin}],
%         Orders = mnesia:select(list_to_atom(Coin ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
%         ok = mnesia:delete({coin, Coin}),
%         ok = rollback_orders(Orders)
%     end),
%     mnesia:delete_table(list_to_atom(Coin ++ "_order")).

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

% ----------------- DEBUG -----------------

% add_deposit(UserId, Quantity) ->
%     UserRecord = #user{id='$1', deposit='$2'},
%     Guard = {'==', '$1', UserId},
%     Users = mnesia:select(user, [{UserRecord, [Guard], ['$_']}]),
%     case Users == [] of
%         true -> 
%             error;
%         false ->
%             [User | _] = Users,
%             NewValue = User#user.deposit + Quantity,
%             ok = mnesia:write(#user{id=User#user.id, deposit=NewValue})
%     end.

insert_new_user(UserId, Deposit) ->
    UserRecord = #user{id='$1', deposit='$2'},
    Guard = {'==', '$1', UserId},
    Users = mnesia:select(user, [{UserRecord, [Guard], ['$_']}]),
    case Users == [] of
        true -> 
            ok = mnesia:write(#user{id=UserId, deposit=Deposit});
        false ->
            error
    end.

insert_new_asset(UserId, CoinId, Quantity) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
    Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
    case Assets == [] of
        true -> 
            ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=Quantity});
        false ->
            error
    end.