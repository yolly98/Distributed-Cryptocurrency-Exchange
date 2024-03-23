-module(main_server_mnesia).

-record(asset_key, {user_id::string(), coin_id::string()}).
-record(asset, {asset_key, quantity::float()}).

-record(coin, {id::string(), value::float()}).

-record(user, {id::string(), deposit::float()}).

-record(order_key, {timestamp::integer(), user_id::string()}).
-record(order, {order_key, type::string(), coin_id::string(), quantity::float()}).

-record(transaction_key, {timestamp::integer(), seller::string(), buyer::string(), coin_id::string()}).
-record(transaction, {transaction_key, coins::float(), market_value::float()}).

-export([
    create_database/0,
    add_node_to_schema/3,
    add_coin/3,
    remove_coin/1
]).

create_database() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
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
        {disc_copies, [node()]},
        {attributes, record_info(fields, transaction)}
    ]),
    
    % test
    add_coin("btc1", 20, []),
    add_coin("btc2", 5, []),
    add_coin("btc3", 0.5, []),

    {atomic, _} = mnesia:transaction(fun() ->
        insert_new_user("Stefano", 500),
        insert_new_user("Andrea", 250),
        insert_new_asset("Andrea", "btc1", 10),
        insert_new_asset("Andrea", "btc2", 23)
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

rollback_orders([]) ->
    ok;

rollback_orders([{_, OrderKey, Type, CoinId, Quantity} | RemainingOrders]) ->
    if 
        Type == "sell" ->
            add_asset(OrderKey#order_key.user_id, CoinId, Quantity);
        Type == "buy" ->
            add_deposit(OrderKey#order_key.user_id, Quantity)
    end,
    rollback_orders(RemainingOrders).

add_asset(UserId, CoinId, Quantity) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
    Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
    case Assets == [] of
        true -> 
            ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=Quantity});
        false ->
            [Asset | _] = Assets,
            NewQuantity = Asset#asset.quantity + Quantity,
            ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=NewQuantity})
    end.

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
    ]).

remove_coin(Coin) ->
    mnesia:transaction(fun() ->
        Table = list_to_atom(Coin ++ "_order"),
        OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
        Guards = [{'==', '$4', Coin}],
        Orders = mnesia:select(list_to_atom(Coin ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
        ok = mnesia:delete({coin, Coin}),
        ok = rollback_orders(Orders)
    end),
    mnesia:delete_table(list_to_atom(Coin ++ "_order")).

% ----------------- DEBUG -----------------

add_deposit(UserId, Quantity) ->
    UserRecord = #user{id='$1', deposit='$2'},
    Guard = {'==', '$1', UserId},
    Users = mnesia:select(user, [{UserRecord, [Guard], ['$_']}]),
    case Users == [] of
        true -> 
            error;
        false ->
            [User | _] = Users,
            NewValue = User#user.deposit + Quantity,
            ok = mnesia:write(#user{id=User#user.id, deposit=NewValue})
    end.

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