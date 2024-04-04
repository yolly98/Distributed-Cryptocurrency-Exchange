-module(coin_node_mnesia).

-record(asset_key, {user_id::string(), coin_id::string()}).
-record(asset, {asset_key, quantity::float()}).

-record(coin, {id::string(), value::float()}).

-record(user, {id::string(), deposit::float()}).

-record(order, {uuid::integer(), timestamp::integer(), user_id::string(), type::string(), coin_id::string(), quantity::float(), limit::float()}).

-record(transaction, {uuid::integer(), timestamp::integer(), seller::string(), buyer::string(), coin_id::string(), coins::float(), market_value::float(), new_market_value::float()}).

-export([
    insert_new_user/2,
    get_coins/0, 
    insert_new_coin/2, 
    update_coin/2,
%    insert_new_asset/3, 
    add_asset/3,
    insert_new_order/5,
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
    get_orders_by_type/4,
    get_assets_by_user/1,
    get_asset_by_user/2,
    get_asset/2,
    sub_asset/3,
    sell/4,
    buy/4
]).

get_uuid(Table) ->
    global:whereis_name(uuid_generator) ! {uuid, Table, self()},
    receive {uuid, UUID} ->
        {ok, UUID}
    end.

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

% insert_new_asset(UserId, CoinId, Quantity) ->
%     AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
%     Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
%     Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
%     case Assets == [] of
%         true -> 
%             ok = mnesia:write(#asset{asset_key=#asset_key{user_id=UserId, coin_id=CoinId}, quantity=Quantity});
%         false ->
%             error
%     end.

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

insert_new_order(UserId, Type, CoinId, Quantity, Limit) ->
    {ok, UUID} = get_uuid(CoinId ++ "_order"),
    Table = list_to_atom(CoinId ++ "_order"),
    Timestamp = os:system_time(nanosecond),
    OrderRecord = {Table, '$1', '$2', '$3', '$4', '$5', '$6', '$7'},
    Guards = [{'==', '$1', UUID}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true -> 
            ok = mnesia:write({Table, UUID, Timestamp, UserId, Type, CoinId, Quantity, Limit}),
            {ok, UUID, Timestamp};
        false ->
            error
    end.

get_pending_orders(UserId, CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, '$1', '$2', '$3', '$4', '$5', '$6', '$7'},
    Guards = [{'==', '$3', UserId}, {'==', '$5', CoinId}],
    Orders = mnesia:select(list_to_atom(CoinId ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
    {ok, Orders}.

get_orders_by_type(UserId, Type, CoinId, MarketValue) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, '$1', '$2', '$3', '$4', '$5', '$6', '$7'},
    if 
        Type == "buy" ->
            Guards = [
                {'=/=', '$3', UserId}, 
                {'==', '$4', Type}, 
                {'==', '$5', CoinId},
                {'orelse', {'==', '$7', 0}, {'>=', '$7', MarketValue}}
            ];
        Type == "sell" ->
            Guards = [
                {'=/=', '$3', UserId}, 
                {'==', '$4', Type}, 
                {'==', '$5', CoinId},
                {'orelse', {'==', '$7', 0}, {'=<', '$7', MarketValue}}
            ]
    end,
    Orders = mnesia:select(list_to_atom(CoinId ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
    {ok, Orders}.

get_orders_by_coin(CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, '$1', '$2', '$3', '$4', '$5', '$6', '$7'},
    Guards = [{'==', '$5', CoinId}],
    Orders = mnesia:select(list_to_atom(CoinId ++ "_order"), [{OrderRecord, Guards, ['$_']}]),
    {ok, Orders}.

update_order_quantity(UUID, CoinId, NewOrderQuantity) -> 
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, '$1', '$2', '$3', '$4', '$5', '$6', '$7'},
    Guards = [{'==', '$1', UUID}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true ->
            error;
        false ->
            [{_, _, Timestamp, UserId, Type, CoinId, _, Limit} | _] = Orders,
            ok = mnesia:write({Table, UUID, Timestamp, UserId, Type, CoinId, NewOrderQuantity, Limit})
    end.

delete_order(UUID, CoinId) ->
    Table = list_to_atom(CoinId ++ "_order"),
    OrderRecord = {Table, '$1', '$2', '$3', '$4', '$5', '$6', '$7'},
    Guards = [{'==', '$1', UUID}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true ->
            error;
        false ->
            [Order | _] = Orders,
            ok = mnesia:delete({list_to_atom(CoinId ++ "_order"), UUID}),
            {ok, Order}
    end.

% -------------------------- TRANSACTION --------------------------

insert_new_transaction(Seller, Buyer, CoinId, Coins, MarketValue, NewMarketValue) ->
    {ok, UUID} = get_uuid("transaction"),
    Timestamp = os:system_time(nanosecond),
    TransactionRecord = #transaction{uuid='$1', timestamp='$2', seller='$3', buyer='$4', coin_id='$5', coins='$6', market_value='$7', new_market_value='$8'},
    Guards = [{'==', '$1', UUID}],
    Transactions = mnesia:select(transaction, [{TransactionRecord, Guards, ['$_']}]),
    case Transactions == [] of
        true -> 
            ok = mnesia:write(#transaction{uuid=UUID, timestamp=Timestamp, seller=Seller, buyer=Buyer, coin_id=CoinId, coins=Coins, market_value=MarketValue, new_market_value=NewMarketValue}),
            {ok, Timestamp};
        false ->
            error
    end.

get_transactions_history(CoinId, Seconds) ->
    MinTimestamp = os:system_time(nanosecond) - (Seconds * math:pow(10, 9)),
    TransactionRecord = #transaction{uuid='$1', timestamp='$2', seller='$3', buyer='$4', coin_id='$5', coins='$6', market_value='$7', new_market_value='$8'},
    Guards = [{'==', '$5', CoinId}, {'>', '$2', MinTimestamp}],
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
    {_, _, _, _, Type, _, Quantity, _} = Order,
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

complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency, Limit) ->
    complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency, Limit, 0, []).

complete_sell_order([], UserId, CoinId, MarketValue, PlacedCurrency, Limit, BoughtAsset, CompletedTransactions) ->
    ok = update_coin(CoinId, MarketValue),
    if 
        PlacedCurrency > 0 -> 
            {ok, PendingOrderUUID, PendingOrderTimestamp} = insert_new_order(UserId, "buy", CoinId, PlacedCurrency, Limit),
            PendingOrder = #{
                <<"uuid">> => list_to_binary(integer_to_list(PendingOrderUUID)),
                <<"timestamp">> => list_to_binary(integer_to_list(PendingOrderTimestamp)),
                <<"quantity">> => PlacedCurrency,
                <<"limit">> => Limit
            };
        PlacedCurrency =< 0 -> 
            PendingOrder = []
    end,
    ok = add_asset(UserId, CoinId, BoughtAsset),
    {ok, CompletedTransactions, PendingOrder};

complete_sell_order([Order | RemainingOrders], UserId, CoinId, MarketValue, PlacedCurrency, Limit, BoughtAsset, CompletedTransactions) ->
    {_, UUID, OrderTimestamp, SellerId, _, _, Quantity, _} = Order,
    {ok, Seller} = get_user(SellerId),
    SellableCurrency = erlang:min(convert_asset_to_currency(MarketValue, Quantity), PlacedCurrency),
    SellableAsset = erlang:min(convert_currency_to_asset(MarketValue, SellableCurrency), Quantity),

    NewOrderQuantity = Quantity - SellableAsset,
    NewSellerDeposit = Seller#user.deposit + SellableCurrency,
    NewMarketValue = update_market_value(MarketValue, "sell", CoinId, SellableAsset),
    NewPlacedCurrency = PlacedCurrency - SellableCurrency,

    if
        NewOrderQuantity == 0 -> {ok, _} = delete_order(UUID, CoinId);
        NewOrderQuantity > 0 -> ok = update_order_quantity(UUID, CoinId, NewOrderQuantity);
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
        <<"order_uuid">> => list_to_binary(integer_to_list(UUID)),
        <<"order_type">> => <<"sell">>,
        <<"order_timestamp">> => list_to_binary(integer_to_list(OrderTimestamp))
    },

    if  
        NewPlacedCurrency == 0 orelse RemainingOrders == [] -> complete_sell_order([], UserId, CoinId, NewMarketValue, NewPlacedCurrency, Limit, BoughtAsset + SellableAsset, CompletedTransactions ++ [CompletedTransaction]);
        NewPlacedCurrency =/= 0 -> complete_sell_order(RemainingOrders, UserId, CoinId, NewMarketValue, NewPlacedCurrency, Limit, BoughtAsset + SellableAsset, CompletedTransactions ++ [CompletedTransaction])
    end.

buy(UserId, CoinId, PlacedCurrency, Limit) ->
    {ok, User} = get_user(UserId),
    Deposit = User#user.deposit,
    if 
        Deposit < PlacedCurrency -> 
            error;
        Deposit >= PlacedCurrency ->
            NewDeposit = Deposit - PlacedCurrency, 
            ok = update_deposit(UserId, NewDeposit),
            {ok, MarketValue} = get_coin_value(CoinId),
            if
                Limit >= MarketValue orelse Limit == 0 ->
                    {ok, Orders} = get_orders_by_type(UserId, "sell", CoinId, MarketValue);
                Limit < MarketValue ->
                    Orders = [],
                    {ok, Orders}
            end,
            {ok, CompletedTransactions, NewPendingOrder} = complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency, Limit),
            {ok, CompletedTransactions, NewPendingOrder}
    end.

complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset, Limit) ->
    complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset, Limit, 0, []).

complete_buy_order([], UserId, CoinId, MarketValue, PlacedAsset, Limit, EarnedCurrency, CompletedTransactions) ->
    ok = update_coin(CoinId, MarketValue),
    {ok, User} = get_user(UserId),
    NewDeposit = User#user.deposit + EarnedCurrency,
    ok = update_deposit(UserId, NewDeposit),
    if 
        PlacedAsset > 0 -> 
            {ok, PendingOrderUUID, PendingOrderTimestamp} = insert_new_order(UserId, "sell", CoinId, PlacedAsset, Limit),
            PendingOrder = #{
                <<"uuid">> => list_to_binary(integer_to_list(PendingOrderUUID)),
                <<"timestamp">> => list_to_binary(integer_to_list(PendingOrderTimestamp)), 
                <<"quantity">> => PlacedAsset,
                <<"limit">> => Limit
            },
            {ok, CompletedTransactions, PendingOrder};
        PlacedAsset == 0 -> 
            {ok, CompletedTransactions, []};
        PlacedAsset < 0 -> 
            error
    end;

complete_buy_order([Order | RemainingOrders], UserId, CoinId, MarketValue, PlacedAsset, Limit, EarnedCurrency, CompletedTransactions) ->
    {_, UUID, OrderTimestamp, BuyerId, _, _, Quantity, _} = Order,
    BuyableAsset = erlang:min(convert_currency_to_asset(MarketValue, Quantity), PlacedAsset),
    BuyableCurrency = erlang:min(convert_asset_to_currency(MarketValue, BuyableAsset), Quantity),

    NewOrderQuantity = Quantity - BuyableCurrency,
    ok = add_asset(BuyerId, CoinId, BuyableAsset), 
    NewMarketValue = update_market_value(MarketValue, "buy", CoinId, BuyableAsset),
    NewPlacedAsset = PlacedAsset - BuyableAsset,

    if
        BuyableCurrency == Quantity -> {ok, _} = delete_order(UUID, CoinId);
        BuyableCurrency < Quantity -> ok = update_order_quantity(UUID, CoinId, NewOrderQuantity);
        BuyableCurrency > Quantity ->
            io:format("~p ~p ~p ~p ~p ~p\n", [BuyableAsset, BuyableCurrency, PlacedAsset, NewPlacedAsset, Quantity, NewOrderQuantity]), % TEST
            ok = error % TEST
    end,

    {ok, Timestamp} = insert_new_transaction(UserId, BuyerId, CoinId, BuyableAsset, MarketValue, NewMarketValue),
    CompletedTransaction = #{
        <<"seller">> => list_to_binary(UserId), 
        <<"buyer">> => list_to_binary(BuyerId),
        <<"coin">> => list_to_binary(CoinId),
        <<"quantity">> => BuyableAsset,
        <<"market_value">> => MarketValue,
        <<"new_market_value">> => NewMarketValue,
        <<"timestamp">> => list_to_binary(integer_to_list(Timestamp)),
        <<"order_uuid">> => list_to_binary(integer_to_list(UUID)),
        <<"order_type">> => <<"buy">>,
        <<"order_timestamp">> => list_to_binary(integer_to_list(OrderTimestamp))
    },

    if  
        NewPlacedAsset == 0 orelse RemainingOrders == [] -> complete_buy_order([], UserId, CoinId, NewMarketValue, NewPlacedAsset, Limit, EarnedCurrency + BuyableCurrency, CompletedTransactions ++ [CompletedTransaction]);
        NewPlacedAsset =/= 0 -> complete_buy_order(RemainingOrders, UserId, CoinId, NewMarketValue, NewPlacedAsset, Limit, EarnedCurrency + BuyableCurrency, CompletedTransactions ++ [CompletedTransaction])
    end.

sell(UserId, CoinId, PlacedAsset, Limit) ->
    {ok, Asset} = get_asset(UserId, CoinId),
    AssetQuantity = Asset#asset.quantity,
    if
        AssetQuantity < PlacedAsset -> 
            error;
        AssetQuantity >= PlacedAsset ->
            ok = sub_asset(UserId, CoinId, PlacedAsset),
            {ok, MarketValue} = get_coin_value(CoinId),

            if
                Limit =< MarketValue orelse Limit == 0 ->
                    {ok, Orders} = get_orders_by_type(UserId, "buy", CoinId, MarketValue);
                Limit > MarketValue ->
                    Orders = [],
                    {ok, Orders}
            end,
            {ok, CompletedTransactions, NewPendingOrder} = complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset, Limit),
            {ok, CompletedTransactions, NewPendingOrder}
    end.
        