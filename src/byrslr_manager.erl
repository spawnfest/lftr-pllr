-module(byrslr_manager).

-behaviour(gen_server).

-export([start_link/0, new_market/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {markets = #{} :: #{atom() => pid()},
                monitors = #{} :: #{reference() => atom()}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

new_market(Id, Options) ->
    gen_server:call(?SERVER, {new_market, Id, Options}).

init({}) ->
    {ok, #state{}}.

handle_call({new_market, Id, _Options}, _From, #state{markets = Markets} = State)
  when is_map_key(Id, Markets) ->
    {reply, {error, already_started}, State};
handle_call({new_market, Id, Options}, _From, #state{markets = Markets, monitors = Monitors} = State) ->
    {ok, MarketPid} = start_market(Id, Options),
    Ref = monitor(process, MarketPid),
    {reply, {ok, MarketPid}, State#state{markets = Markets#{Id => MarketPid},
                                         monitors = Monitors#{Ref => Id}}};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, _}, State) ->
    Id = maps:get(Ref, State#state.monitors),
    NewMonitors = maps:remove(Ref, State#state.monitors),
    NewMarkets = maps:remove(Id, State#state.markets),
    {noreply, State#state{markets = NewMarkets, monitors = NewMonitors}}.

start_market(Id, Options) ->
    {ok, MarketPid} = market_sup:start_market(Id, Options).
    %% TODO handle periodic markets by starting a market director.
    %% case proplists:get(market_period, Options, nil) of
    %%     nil ->
