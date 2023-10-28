-module(gen_market).

-behaviour(gen_statem).

-export([start_link/2, open_market/1]).
-export([callback_mode/0, init/1, handle_event/4]).

-record(data, {periodic = nil :: nil | #{}}).

open_market(Market) ->
    gen_statem:cast(Market, open_market).

start_link(Id, Options) ->
    gen_statem:start_link({local, Id}, ?MODULE, Options, []).

init(Options) ->
    Periodic = periodic_options(Options),
    {ok, closed, #data{periodic = Periodic}}.

periodic_options(Options) ->
    case proplists:get_value(market_period, Options, nil) of
        nil ->
            nil;
        MarketPeriod ->
            #{market_period => MarketPeriod,
              reservation_delay => proplists:get_value(reservation_delay, Options, 0),
              offer_delay => proplists:get_value(offer_delay, Options, MarketPeriod div 3)}
    end.

callback_mode() -> handle_event_function.

handle_event(cast, open_market, closed, Data) ->
    {next_state, accept_reservations, Data}.
