-module(coin_node_mnesia).

-record(asset_key, {user_id::string(), coin_id::string()}).
-record(asset, {asset_key, quantity::float()}).

-record(coin, {id::string(), value::float()}).

-record(user, {id::string(), deposit::float()}).

-record(order_key, {timestamp::integer(), user_id::string()}).

-record(transaction_key, {timestamp::integer(), seller::string(), buyer::string(), coin_id::string()}).
-record(transaction, {transaction_key, coins::float(), market_value::float(), new_market_value::float()}).

-export([
    insert_new_user/2,
    get_coins/0, 
    insert_new_coin/2, 
    update_coin/2,
    insert_new_asset/3, 
    add_asset/3,
    insert_new_order/4,
    update_order_quantity/3,
    delete_order/2, 
    insert_new_transaction/6,
    get_transactions_history/2,
    get_user/1,
    get_deposit/1,
    add_deposit/2,
    sub_deposit/2,
    update_deposit/2,
    get_coin_value/1,
    get_pending_orders/2,
    get_orders_by_type/3,
    get_assets_by_user/1,
    get_asset_by_user/2,
    get_asset/2,
    sub_asset/3,
    sell/3,
    buy/3
]).

% -------------------------- USER --------------------------

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

get_user(UserId) ->
    UserRecord = #user{id='$1', deposit='$2'},
    Guard = {'==', '$1', UserId},
    Users = mnesia:select(user, [{UserRecord, [Guard], ['$_']}]),
    case Users == [] of
        true -> 
            error;
        false ->
            [User | _] = Users,
            {ok, User}
    end.

get_deposit(UserId) ->
    UserRecord = #user{id='$1', deposit='$2'},
    Guard = {'==', '$1', UserId},
    Deposits = mnesia:select(user, [{UserRecord, [Guard], ['$2']}]),
    case Deposits == [] of
        true -> 
            error;
        false ->
            [Deposit | _] = Deposits,
            {ok, Deposit}
    end.

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

sub_deposit(UserId, Quantity) ->
    UserRecord = #user{id='$1', deposit='$2'},
    Guard = {'==', '$1', UserId},
    Users = mnesia:select(user, [{UserRecord, [Guard], ['$_']}]),
    case Users == [] of
        true -> 
            error;
        false ->
            [User | _] = Users,
            NewValue = User#user.deposit - Quantity,
            if 
                NewValue >= 0 ->
                    ok = mnesia:write(#user{id=User#user.id, deposit=NewValue});
                NewValue < 0 ->
                    error
            end
    end.

update_deposit(UserId, NewValue) ->
    UserRecord = #user{id='$1', deposit='$2'},
    Guard = {'==', '$1', UserId},
    Users = mnesia:select(user, [{UserRecord, [Guard], ['$_']}]),
    case Users == [] of
        true -> 
            error;
        false ->
            [User | _] = Users,
            ok = mnesia:write(#user{id=User#user.id, deposit=NewValue})
    end.

% -------------------------- COIN --------------------------

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

get_coin_value(CoinId) ->
    CoinRecord = #coin{id='$1', value='$2'},
    Guard = {'==', '$1', CoinId},
    Values = mnesia:select(coin, [{CoinRecord, [Guard], ['$2']}]),
    case Values == [] of
        true -> 
            error;
        false ->
            [Value | _] = Values,
            {ok, Value}
    end.

get_coins() ->
    CoinRecord = #coin{id='$1', value='$2'},
    Coins = mnesia:select(coin, [{CoinRecord, [], ['$_']}]),
    {ok, Coins}.

update_coin(CoinId, MarketValue) -> 
    CoinRecord = #coin{id='$1', value='$2'},
    Guard = {'==', '$1', CoinId},
    Coins = mnesia:select(coin, [{CoinRecord, [Guard], ['$_']}]),
    case Coins == [] of
        true -> 
            error;
        false ->
            ok = mnesia:write(#coin{id=CoinId, value=MarketValue})
    end.

% -------------------------- ASSET --------------------------

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

prepare_assets_list([], PreparedAssetsList) ->
    {ok, PreparedAssetsList};

prepare_assets_list([[CoinId, Quantity] | RemainingAssets], PreparedAssetsList) ->
    Asset = #{<<"coin">> => list_to_binary(CoinId), <<"quantity">> => Quantity},
    prepare_assets_list(RemainingAssets, PreparedAssetsList ++ [Asset]).

get_assets_by_user(UserId) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}],
    Assets = mnesia:select(asset, [{AssetRecord, Guards, [['$2', '$3']]}]),
    prepare_assets_list(Assets, []).

get_asset_by_user(UserId, CoinId) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
    Assets  = mnesia:select(asset, [{AssetRecord, Guards, ['$3']}]),
    case Assets == [] of
        true -> 
            {ok, []};
        false ->
            [Asset | _] = Assets,
            {ok, Asset}
    end.

get_asset(UserId, CoinId) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
    Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
    case Assets == [] of
        true -> 
            {error, []};
        false ->
            [Asset | _] = Assets,
            {ok, Asset}
    end.

sub_asset(UserId, CoinId, Quantity) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
    Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
    case Assets == [] of
        true -> 
            error;
        false ->
            [Asset | _] = Assets,
            NewQuantity = Asset#asset.quantity - Quantity,
            if 
                NewQuantity >= 0 ->
                    ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=NewQuantity});
                NewQuantity < 0 -> 
                    error
            end
    end.

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

% -------------------------- ORDER --------------------------

insert_new_order(UserId, Type, CoinId, Quantity) ->
    Table = list_to_atom(CoinId ++ "_order"),
    Timestamp = os:system_time(nanosecond),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'==', '$1', Timestamp}, {'==', '$2', UserId}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true -> 
            ok = mnesia:write({Table, {order_key, Timestamp, UserId}, Type, CoinId, Quantity}),
            {ok, Timestamp};
        false ->
            error
    end.

get_pending_orders(UserId, CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'==', '$2', UserId}, {'==', '$4', CoinId}],
    Orders = mnesia:select(list_to_atom(CoinId ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
    {ok, Orders}.

get_orders_by_type(UserId, Type, CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'=/=', '$2', UserId}, {'==', '$3', Type}, {'==', '$4', CoinId}],
    Orders = mnesia:select(list_to_atom(CoinId ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
    {ok, Orders}.

get_orders_by_coin(CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'==', '$4', CoinId}],
    Orders = mnesia:select(list_to_atom(CoinId ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
    {ok, Orders}.

update_order_quantity(OrderKey, CoinId, NewOrderQuantity) -> 
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'==', '$1', OrderKey#order_key.timestamp}, {'==', '$2', OrderKey#order_key.user_id}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true ->
            error;
        false ->
            [{_, _, Type, CoinId, _} | _] = Orders,
            ok = mnesia:write({Table, OrderKey, Type, CoinId, NewOrderQuantity})
    end.

delete_order(OrderKey, CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'==', '$1', OrderKey#order_key.timestamp}, {'==', '$2', OrderKey#order_key.user_id}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true ->
            error;
        false ->
            [Order | _] = Orders,
            ok = mnesia:delete({list_to_atom(CoinId ++ "_order"), OrderKey}),
            {ok, Order}
    end.

% -------------------------- TRANSACTION --------------------------

insert_new_transaction(Seller, Buyer, CoinId, Coins, MarketValue, NewMarketValue) ->
    Timestamp = os:system_time(nanosecond),
    TransactionRecord = #transaction{transaction_key=#transaction_key{timestamp='$1', seller='$2', buyer='$3', coin_id='$4'}, coins='$5', market_value='$6', new_market_value='$7'},
    Guards = [{'==', '$1', Timestamp}, {'==', '$2', Seller}, {'==', '$3', Buyer}, {'==', '$4', CoinId}],
    Transactions = mnesia:select(transaction, [{TransactionRecord, Guards, ['$_']}]),
    case Transactions == [] of
        true -> 
            ok = mnesia:write(#transaction{transaction_key=#transaction_key{timestamp=Timestamp, seller=Seller, buyer=Buyer, coin_id=CoinId}, coins=Coins, market_value=MarketValue, new_market_value=NewMarketValue}),
            {ok, Timestamp};
        false ->
            error
    end.

get_transactions_history(CoinId, Seconds) ->
    MinTimestamp = os:system_time(nanosecond) - (Seconds * math:pow(10, 9)),
    TransactionRecord = #transaction{transaction_key=#transaction_key{timestamp='$1', seller='$2', buyer='$3', coin_id='$4'}, coins='$5', market_value='$6', new_market_value='$7'},
    Guards = [{'==', '$4', CoinId}, {'>', '$1', MinTimestamp}],
    Transactions = mnesia:select(transaction, [{TransactionRecord, Guards, ['$_']}]),
    {ok, Transactions}.

% -------------------------- BUSINESS LOGIC --------------------------

convert_asset_to_currency(MarketValue, Quantity) ->
    MarketValue * Quantity.

convert_currency_to_asset(MarketValue, Quantity) ->
    Quantity / MarketValue.

count_orders([], TotalBuy, TotalSell) ->
    {TotalBuy, TotalSell};

count_orders([Order | RemainingOrders], TotalBuy, TotalSell) ->
    {_, _, Type, _, Quantity} = Order,
    if
        Type == "buy" -> 
            count_orders(RemainingOrders, TotalBuy + Quantity, TotalSell);
        Type == "sell" -> 
            count_orders(RemainingOrders, TotalBuy, TotalSell + Quantity)
    end.

update_market_value(MarketValue, Type, CoinId, Quantity) ->
    {ok, Orders} = get_orders_by_coin(CoinId),
    {TotalBuy, TotalSell} = count_orders(Orders, 0, 0),
    PendingWeigth = (convert_currency_to_asset(MarketValue, TotalBuy) - TotalSell) * 0.0001,
    if 
        Type == "buy" ->
            NewMarketValue = MarketValue + Quantity * MarketValue * 0.001 + PendingWeigth;
        Type == "sell" ->
            NewMarketValue = MarketValue - Quantity * MarketValue * 0.001 + PendingWeigth
    end,
    NewMarketValue.

complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency) ->
    complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency, 0, []).

complete_sell_order([], UserId, CoinId, MarketValue, PlacedCurrency, BoughtAsset, CompletedTransactions) ->
    ok = update_coin(CoinId, MarketValue),
    if 
        PlacedCurrency > 0 -> 
            {ok, PendingOrderTimestamp} = insert_new_order(UserId, "buy", CoinId, PlacedCurrency),
            PendingOrder = #{
                <<"timestamp">> => list_to_binary(integer_to_list(PendingOrderTimestamp)),
                <<"quantity">> => PlacedCurrency
            };
        PlacedCurrency =< 0 -> 
            PendingOrder = []
    end,
    ok = add_asset(UserId, CoinId, BoughtAsset),
    {ok, CompletedTransactions, PendingOrder};

complete_sell_order([Order | RemainingOrders], UserId, CoinId, MarketValue, PlacedCurrency, BoughtAsset, CompletedTransactions) ->
    {_, OrderKey, _, _, Quantity} = Order,
    {ok, Seller} = get_user(OrderKey#order_key.user_id),
    SellableCurrency = erlang:min(convert_asset_to_currency(MarketValue, Quantity), PlacedCurrency),
    SellableAsset = erlang:min(convert_currency_to_asset(MarketValue, SellableCurrency), Quantity),

    NewOrderQuantity = Quantity - SellableAsset,
    NewSellerDeposit = Seller#user.deposit + SellableCurrency,
    NewMarketValue = update_market_value(MarketValue, "sell", CoinId, SellableAsset),
    NewPlacedCurrency = PlacedCurrency - SellableCurrency,

    if
        NewOrderQuantity == 0 -> {ok, _} = delete_order(OrderKey, CoinId);
        NewOrderQuantity > 0 -> ok = update_order_quantity(OrderKey, CoinId, NewOrderQuantity);
        NewOrderQuantity < 0 -> 
            io:format("~p ~p ~p ~p ~p ~p\n", [SellableAsset, SellableCurrency, PlacedCurrency, NewPlacedCurrency, Quantity, NewOrderQuantity]), % TEST
            ok = error % TEST
    end,
    ok = update_deposit(Seller#user.id, NewSellerDeposit),
    {ok, Timestamp} = insert_new_transaction(Seller#user.id, UserId, CoinId, SellableAsset, MarketValue, NewMarketValue),
    CompletedTransaction = #{
        <<"seller">> => list_to_binary(Seller#user.id), 
        <<"buyer">> => list_to_binary(UserId),
        <<"coin">> => list_to_binary(CoinId),
        <<"quantity">> => SellableAsset,
        <<"market_value">> => MarketValue,
        <<"new_market_value">> => NewMarketValue,
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"order_type">> => <<"sell">>,
        <<"order_timestamp">> => list_to_binary(integer_to_list(OrderKey#order_key.timestamp))
    },

    if  
        NewPlacedCurrency == 0 orelse RemainingOrders == [] -> complete_sell_order([], UserId, CoinId, NewMarketValue, NewPlacedCurrency, BoughtAsset + SellableAsset, CompletedTransactions ++ [CompletedTransaction]);
        NewPlacedCurrency =/= 0 -> complete_sell_order(RemainingOrders, UserId, CoinId, NewMarketValue, NewPlacedCurrency, BoughtAsset + SellableAsset, CompletedTransactions ++ [CompletedTransaction])
    end.

buy(UserId, CoinId, PlacedCurrency) ->
    {ok, User} = get_user(UserId),
    Deposit = User#user.deposit,
    if 
        Deposit < PlacedCurrency -> 
            error;
        Deposit >= PlacedCurrency ->
            NewDeposit = Deposit - PlacedCurrency, 
            ok = update_deposit(UserId, NewDeposit),
            {ok, MarketValue} = get_coin_value(CoinId),
            {ok, Orders} = get_orders_by_type(UserId, "sell", CoinId),

            {ok, CompletedTransactions, NewPendingOrder} = complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency),
            {ok, CompletedTransactions, NewPendingOrder}
    end.

complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset) ->
    complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset, 0, []).

complete_buy_order([], UserId, CoinId, MarketValue, PlacedAsset, EarnedCurrency, CompletedTransactions) ->
    ok = update_coin(CoinId, MarketValue),
    {ok, User} = get_user(UserId),
    NewDeposit = User#user.deposit + EarnedCurrency,
    ok = update_deposit(UserId, NewDeposit),
    if 
        PlacedAsset > 0 -> 
            {ok, PendingOrderTimestamp} = insert_new_order(UserId, "sell", CoinId, PlacedAsset),
            PendingOrder = #{
                <<"timestamp">> => list_to_binary(integer_to_list(PendingOrderTimestamp)), 
                <<"quantity">> => PlacedAsset
            },
            {ok, CompletedTransactions, PendingOrder};
        PlacedAsset == 0 -> 
            {ok, CompletedTransactions, []};
        PlacedAsset < 0 -> 
            error
    end;

complete_buy_order([Order | RemainingOrders], UserId, CoinId, MarketValue, PlacedAsset, EarnedCurrency, CompletedTransactions) ->
    {_, OrderKey, _, _, Quantity} = Order,
    BuyableAsset = erlang:min(convert_currency_to_asset(MarketValue, Quantity), PlacedAsset),
    BuyableCurrency = erlang:min(convert_asset_to_currency(MarketValue, BuyableAsset), Quantity),

    NewOrderQuantity = Quantity - BuyableCurrency,
    ok = add_asset(OrderKey#order_key.user_id, CoinId, BuyableAsset), 
    NewMarketValue = update_market_value(MarketValue, "buy", CoinId, BuyableAsset),
    NewPlacedAsset = PlacedAsset - BuyableAsset,

    if
        BuyableCurrency == Quantity -> {ok, _} = delete_order(OrderKey, CoinId);
        BuyableCurrency < Quantity -> ok = update_order_quantity(OrderKey, CoinId, NewOrderQuantity);
        BuyableCurrency > Quantity ->
            io:format("~p ~p ~p ~p ~p ~p\n", [BuyableAsset, BuyableCurrency, PlacedAsset, NewPlacedAsset, Quantity, NewOrderQuantity]), % TEST
            ok = error % TEST
    end,

    {ok, Timestamp} = insert_new_transaction(UserId, OrderKey#order_key.user_id, CoinId, BuyableAsset, MarketValue, NewMarketValue),
    CompletedTransaction = #{
        <<"seller">> => list_to_binary(UserId), 
        <<"buyer">> => list_to_binary(OrderKey#order_key.user_id),
        <<"coin">> => list_to_binary(CoinId),
        <<"quantity">> => BuyableAsset,
        <<"market_value">> => MarketValue,
        <<"new_market_value">> => NewMarketValue,
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"order_type">> => <<"buy">>,
        <<"order_timestamp">> => list_to_binary(integer_to_list(OrderKey#order_key.timestamp))
    },

    if  
        NewPlacedAsset == 0 orelse RemainingOrders == [] -> complete_buy_order([], UserId, CoinId, NewMarketValue, NewPlacedAsset, EarnedCurrency + BuyableCurrency, CompletedTransactions ++ [CompletedTransaction]);
        NewPlacedAsset =/= 0 -> complete_buy_order(RemainingOrders, UserId, CoinId, NewMarketValue, NewPlacedAsset, EarnedCurrency + BuyableCurrency, CompletedTransactions ++ [CompletedTransaction])
    end.

sell(UserId, CoinId, PlacedAsset) ->
    {ok, Asset} = get_asset(UserId, CoinId),
    AssetQuantity = Asset#asset.quantity,
    if
        AssetQuantity < PlacedAsset -> 
            error;
        AssetQuantity >= PlacedAsset ->
            ok = sub_asset(UserId, CoinId, PlacedAsset),
            {ok, Orders} = get_orders_by_type(UserId, "buy", CoinId),
            {ok, MarketValue} = get_coin_value(CoinId),

            {ok, CompletedTransactions, NewPendingOrder} = complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset),
            {ok, CompletedTransactions, NewPendingOrder}
    end.
        