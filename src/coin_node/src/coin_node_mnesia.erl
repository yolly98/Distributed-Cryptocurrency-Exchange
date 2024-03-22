-module(coin_node_mnesia).

-record(asset_key, {user_id::string(), coin_id::string()}).
-record(asset, {asset_key, quantity::float()}).

-record(coin, {id::string(), value::float()}).

-record(user, {id::string(), deposit::float()}).

-record(order_key, {timestamp::integer(), user_id::string()}).
-record(order, {order_key, type::string(), coin_id::string(), quantity::float()}).

-record(transaction_key, {timestamp::integer(), seller::string(), buyer::string(), coin_id::string()}).
-record(transaction, {transaction_key, coins::float(), market_value::float()}).

-export([
    insert_new_user/2, 
    insert_new_coin/2, 
    update_coin/2,
    insert_new_asset/3, 
    add_asset/3,
    insert_new_order/4,
    update_order_quantity/3,
    delete_order/2, 
    insert_new_transaction/5,
    get_user/1,
    update_deposit/2,
    get_coin_value/1,
    get_orders_by_type/3,

    get_asset/2,
    sub_asset/3,
    sell/3,

    buy/3]).

% mnesia:transaction(fun() ->  mnesia:select(user, [{'_', [], ['$_']}]) end).

% removeTable() ->
%    mnesia:delete_table(person).

round_decimal(Number, Precision) -> %TODO spostare su util
    Factor = math:pow(10, Precision),
    round(Number * Factor) / Factor.

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

get_asset(UserId, CoinId) ->
    AssetRecord = #asset{asset_key=#asset_key{user_id='$1', coin_id='$2'}, quantity='$3'},
    Guards = [{'==', '$1', UserId}, {'==', '$2', CoinId}],
    Assets = mnesia:select(asset, [{AssetRecord, Guards, ['$_']}]),
    case Assets == [] of
        true -> 
            error;
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
            NewQuantity = round_decimal(Asset#asset.quantity - Quantity, 8),
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

insert_new_order(UserId, Type, CoinId, Quantity) ->
    Table = list_to_atom(CoinId ++ "_order"),
    Timestamp = os:system_time(nanosecond),
    OrderRecord = {Table, {order_key, '$1', '$2'}, '$3', '$4', '$5'},
    Guards = [{'==', '$1', Timestamp}, {'==', '$2', UserId}],
    Orders = mnesia:select(Table, [{OrderRecord, Guards, ['$_']}]),
    case Orders == [] of
        true -> 
            ok = mnesia:write({Table, {order_key, Timestamp, UserId}, Type, CoinId, Quantity});
        false ->
            error
    end.

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
    ok = mnesia:delete({list_to_atom(CoinId ++ "_order"), OrderKey}).

insert_new_transaction(Seller, Buyer, CoinId, Coins, MarketValue) ->
    Timestamp = os:system_time(nanosecond),
    TransactionRecord = #transaction{transaction_key=#transaction_key{timestamp='$1', seller='$2', buyer='$3', coin_id='$4'}, coins='$5', market_value='$6'},
    Guards = [{'==', '$1', Timestamp}, {'==', '$2', Seller}, {'==', '$3', Buyer}, {'==', '$4', CoinId}],
    Transactions = mnesia:select(transaction, [{TransactionRecord, Guards, ['$_']}]),
    case Transactions == [] of
        true -> 
            ok = mnesia:write(#transaction{transaction_key=#transaction_key{timestamp=Timestamp, seller=Seller, buyer=Buyer, coin_id=CoinId}, coins=Coins, market_value=MarketValue});
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

get_coin_value(CoinId) ->
    Coin = #coin{id='$1', value='$2'},
    Guard = {'==', '$1', CoinId},
    Values = mnesia:select(coin, [{Coin, [Guard], ['$2']}]),
    case Values == [] of
        true -> 
            error;
        false ->
            [Value | _] = Values,
            {ok, Value}
    end.

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
    complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency, 0).

complete_sell_order([], UserId, CoinId, MarketValue, PlacedCurrency, BoughtAsset) ->
    ok = update_coin(CoinId, MarketValue),
    if 
        PlacedCurrency > 0 -> ok = insert_new_order(UserId, "buy", CoinId, PlacedCurrency);
        PlacedCurrency =< 0 -> ok
    end,
    ok = add_asset(UserId, CoinId, BoughtAsset),
    ok;

complete_sell_order([Order | RemainingOrders], UserId, CoinId, MarketValue, PlacedCurrency, BoughtAsset) ->
    {_, OrderKey, _, _, Quantity} = Order,
    {ok, Seller} = get_user(OrderKey#order_key.user_id),
    SellableCurrency = erlang:min(convert_asset_to_currency(MarketValue, Quantity), PlacedCurrency),
    SellableAsset = convert_currency_to_asset(MarketValue, SellableCurrency),

    NewOrderQuantity = round_decimal(Quantity - SellableAsset, 8),
    NewSellerDeposit = Seller#user.deposit + SellableCurrency,
    NewMarketValue = update_market_value(MarketValue, "sell", CoinId, SellableAsset),
    NewPlacedCurrency = round_decimal(PlacedCurrency - SellableCurrency, 8),

    if
        NewOrderQuantity == 0 -> ok = delete_order(OrderKey, CoinId);
        NewOrderQuantity > 0 -> ok = update_order_quantity(OrderKey, CoinId, NewOrderQuantity);
        NewOrderQuantity < 0 -> error
    end,
    ok = update_deposit(Seller#user.id, NewSellerDeposit),
    ok = insert_new_transaction(Seller#user.id, UserId, CoinId, SellableAsset, MarketValue),

    if  
        NewPlacedCurrency == 0 orelse RemainingOrders == [] -> complete_sell_order([], UserId, CoinId, NewMarketValue, NewPlacedCurrency, BoughtAsset + SellableAsset);
        NewPlacedCurrency =/= 0 -> complete_sell_order(RemainingOrders, UserId, CoinId, NewMarketValue, NewPlacedCurrency, BoughtAsset + SellableAsset)
    end.

buy(UserId, CoinId, PlacedCurrency) ->
    {ok, User} = get_user(UserId),
    Deposit = User#user.deposit,
    if 
        Deposit < PlacedCurrency -> error;
        Deposit >= PlacedCurrency ->
            NewDeposit = round_decimal(Deposit - PlacedCurrency, 8), 
            ok = update_deposit(UserId, NewDeposit),
            {ok, MarketValue} = get_coin_value(CoinId),
            {ok, Orders} = get_orders_by_type(UserId, "sell", CoinId),

            complete_sell_order(Orders, UserId, CoinId, MarketValue, PlacedCurrency)
    end.

complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset) ->
    complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset, 0).

complete_buy_order([], UserId, CoinId, MarketValue, PlacedAsset, EarnedCurrency) ->
    ok = update_coin(CoinId, MarketValue),
    {ok, User} = get_user(UserId),
    NewDeposit = User#user.deposit + EarnedCurrency,
    update_deposit(UserId, NewDeposit),
    if 
        PlacedAsset > 0 -> ok = insert_new_order(UserId, "sell", CoinId, PlacedAsset);
        PlacedAsset == 0 -> ok;
        PlacedAsset < 0 -> error
    end,
    ok;

complete_buy_order([Order | RemainingOrders], UserId, CoinId, MarketValue, PlacedAsset, EarnedCurrency) ->
    {_, OrderKey, _, _, Quantity} = Order,
    BuyableAsset = convert_currency_to_asset(MarketValue, Quantity),
    BuyableCurrency = convert_asset_to_currency(MarketValue, erlang:min(BuyableAsset, PlacedAsset)),

    NewOrderQuantity = round_decimal(Quantity - BuyableCurrency, 8),
    ok = add_asset(OrderKey#order_key.user_id, CoinId, BuyableAsset), 
    NewMarketValue = update_market_value(MarketValue, "buy", CoinId, BuyableAsset),
    NewPlacedAsset = round_decimal(PlacedAsset - BuyableAsset, 8),

    if
        BuyableCurrency == Quantity -> ok = delete_order(OrderKey, CoinId);
        BuyableCurrency < Quantity -> ok = update_order_quantity(OrderKey, CoinId, NewOrderQuantity)
    end,

    ok = insert_new_transaction(UserId, OrderKey#order_key.user_id, CoinId, BuyableAsset, MarketValue),

    if  
        NewPlacedAsset == 0 orelse RemainingOrders == [] -> complete_buy_order([], UserId, CoinId, NewMarketValue, NewPlacedAsset, EarnedCurrency + BuyableCurrency);
        NewPlacedAsset =/= 0 -> complete_buy_order(RemainingOrders, UserId, CoinId, NewMarketValue, NewPlacedAsset, EarnedCurrency + BuyableCurrency)
    end.

sell(UserId, CoinId, PlacedAsset) ->
    {ok, Asset} = get_asset(UserId, CoinId),
    AssetQuantity = Asset#asset.quantity,
    if
        AssetQuantity < PlacedAsset -> error;
        AssetQuantity >= PlacedAsset ->
            ok = sub_asset(UserId, CoinId, PlacedAsset),
            {ok, Orders} = get_orders_by_type(UserId, "buy", CoinId),
            {ok, MarketValue} = get_coin_value(CoinId),

            complete_buy_order(Orders, UserId, CoinId, MarketValue, PlacedAsset)
    end.

    


        