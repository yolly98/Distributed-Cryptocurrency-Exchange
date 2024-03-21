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
    insert_new_coin/2,
    get_coins/0,
    create_order_table/1
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
    {atomic, _} = mnesia:transaction(fun() ->
        insert_new_coin("btc1", 20),
        insert_new_coin("btc2", 20),
        insert_new_coin("btc3", 20),

        create_order_table("btc1"),
        create_order_table("btc2"),
        create_order_table("btc3"),

        insert_new_user("Stefano", 500),
        insert_new_user("Andrea", 250),
        insert_new_asset("Andrea", "btc1", 10)
    end).

create_order_table(Coin) ->
    mnesia:create_table(list_to_atom(Coin ++ "_order"), [
        {type, ordered_set},
        {disc_copies, [node()]},
        {attributes, record_info(fields, order)}
    ]).


% ------------------------------------

get_coins() ->
    {atomic, Res} = mnesia:transaction(fun() ->
        CoinRecord = #coin{id='$1', value='$2'},
        Coins = mnesia:select(coin, [{CoinRecord, [], ['$1']}]),
        case Coins == [] of
            true -> 
                error;
            false ->
                {ok, Coins}
        end
    end),
    Res.

insert_new_coin(CoinId, Value) ->
    CoinRecord = #coin{id='$1', value='$2'},
    Guard = {'==', '$1', CoinId},
    Coins = mnesia:select(coin, [{CoinRecord, [Guard], ['$_']}]),
    case Coins == [] of
        true -> 
            ok = mnesia:write(#coin{id=CoinId, value=Value});
        false ->
            error
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