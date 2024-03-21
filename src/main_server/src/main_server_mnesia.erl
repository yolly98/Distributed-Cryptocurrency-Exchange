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
    get_coins/0
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

    mnesia:create_table(order, [
        {type, ordered_set},
        {disc_copies, [node()]},
        {attributes, record_info(fields, order)}
    ]),

    mnesia:create_table(transaction, [
        {disc_copies, [node()]},
        {attributes, record_info(fields, transaction)}
    ]),
    
    insert_new_coin("btc1", 20),
    insert_new_coin("btc2", 20),
    insert_new_coin("btc3", 20),
    insert_new_coin("btc4", 20),
    insert_new_coin("btc5", 20),
    insert_new_coin("btc6", 20),
    insert_new_coin("btc7", 20).


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


% write(Tab :: table(),
%       Record :: tuple(),
%       LockKind :: write_locks()) ->
%          ok
insert_new_coin(CoinId, Value) ->
    {atomic, Res} = mnesia:transaction(fun() ->
        CoinRecord = #coin{id='$1', value='$2'},
        Guard = {'==', '$1', CoinId},
        Coins = mnesia:select(coin, [{CoinRecord, [Guard], ['$_']}]),
        case Coins == [] of
            true -> 
                ok = mnesia:write(#coin{id=CoinId, value=Value});
            false ->
                error
        end
    end),
    Res.