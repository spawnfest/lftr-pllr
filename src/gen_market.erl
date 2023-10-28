-module(gen_market).

-behaviour(gen_statem).

-export([start_link/2, open_market/1, accept_reservations/1, accept_offers/1]).
-export([callback_mode/0, init/1, handle_event/4]).

-record(data, {periodic = nil :: nil | #{},
               market_period :: undefined | timer:tref(),
               reservation_delay :: undefined | timer:tref(),
               offer_delay :: undefined | timer:tref()}).

open_market(Market) ->
    gen_statem:cast(Market, open_market).

accept_reservations(Market) ->
    gen_statem:cast(Market, accept_reservations).

accept_offers(Market) ->
    gen_statem:cast(Market, accept_offers).

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

handle_event(cast, open_market, closed, #data{periodic = nil} = Data) ->
    {next_state, open, Data};
handle_event(cast, open_market, closed, #data{periodic = Periodic} = Data) ->
    {ok, MarketPeriodTimer} =
        timer:apply_interval(
          maps:get(market_period, Periodic), gen_market, open_market, [self()]),
    {ReservationDelayTimer, OfferDelayTimer} = start_timers(Periodic),
    {next_state, open, Data#data{market_period = MarketPeriodTimer,
                                 reservation_delay = ReservationDelayTimer,
                                 offer_delay = OfferDelayTimer}}.

start_timers(#{reservation_delay := ReservationDelay,
               offer_delay := OfferDelay}) ->
    {ok, ReservationDelayTimer} =
        timer:apply_after(ReservationDelay, gen_market, accept_reservations, [self()]),
    {ok, OfferDelayTimer} =
        timer:apply_after(OfferDelay, gen_market, accept_offers, [self()]),
    {ReservationDelayTimer, OfferDelayTimer}.
