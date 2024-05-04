-module(coin_node_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

create_order_fillers([], ChildSpecs) ->
    {ok, ChildSpecs};

create_order_fillers([{_, Coin, _} | RemainingCoins], ChildSpecs) ->
    NewChild = #{
        id => list_to_atom(Coin ++ "_order_filler"),
        start => {order_filler, start, [Coin]},
        restart => permanent,
        shutdown => brutal_kill
    },
    create_order_fillers(RemainingCoins, ChildSpecs ++ [NewChild]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 5},
    BrodcastDispatcher = #{
        id => dispatcher,
        start => {broadcast_dispatcher, start, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [broadcast_dispatcher]
    },
    CowboyListener = #{
        id => listener,
        start => {cowboy_listener, start, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [cowboy_listener]
    },
    ChildSpecs = [BrodcastDispatcher, CowboyListener],

    % create one order_filler for each coin
    {atomic, Coins} = mnesia:transaction(fun() ->
        {ok, Coins} = coin_node_mnesia:get_coins(),
        Coins
    end),
    {ok, NewChildSpecs} = create_order_fillers(Coins, ChildSpecs),
    
    {ok, {SupFlags, NewChildSpecs}}.
