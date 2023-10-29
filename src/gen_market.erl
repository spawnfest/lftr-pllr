-module(gen_market).

-behaviour(gen_statem).

-export([start_link/2, open_market/1, accept_reservations/1, accept_offers/1, join/1,
         make_reservation/2, make_offer/3]).
-export([callback_mode/0, init/1, handle_event/4]).

-record(data, {periodic = nil :: nil | #{},
               market_period :: undefined | timer:tref(),
               reservation_delay :: undefined | timer:tref(),
               offer_delay :: undefined | timer:tref(),
               buy_offers = [] :: [any()],
               sell_offers = [] :: [any()],
               subscribers = [] :: [pid()],
               market_impl :: module(),
               mcp :: any()}).

-callback clear(BuyOffers :: [any()], SellOffers :: [any()]) -> {ok, MCP :: any()} | {erorr, Reason :: any()}.

open_market(Market) ->
    gen_statem:cast(Market, open_market).

accept_reservations(Market) ->
    gen_statem:cast(Market, accept_reservations).

accept_offers(Market) ->
    gen_statem:cast(Market, accept_offers).

join(Market) ->
    gen_statem:cast(Market, {subscribe, self()}).

make_reservation(MarketId, ByrSlr) ->
    gen_statem:call(MarketId, {make_reservation, ByrSlr}).

make_offer(MarketId, Id, Offer) ->
    gen_statem:call(MarketId, {make_offer, Id, Offer}).

start_link(Id, Options) ->
    gen_statem:start_link({local, Id}, ?MODULE, Options, []).

init(Options) ->
    Periodic = periodic_options(Options),
    MarketImpl = proplists:get_value(market_impl, Options, byrslr_basic_market),
    {ok, closed, #data{periodic = Periodic, market_impl = MarketImpl}}.

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
    notify(Data#data.subscribers, market_open),
    {next_state, open, Data};
handle_event(cast, open_market, closed, #data{periodic = Periodic} = Data) ->
    {ok, MarketPeriodTimer} =
        timer:apply_interval(
          maps:get(market_period, Periodic), gen_market, open_market, [self()]),
    {ReservationDelayTimer, OfferDelayTimer} = start_timers(Periodic),
    notify(Data#data.subscribers, market_open),
    {next_state, open, Data#data{market_period = MarketPeriodTimer,
                                 reservation_delay = ReservationDelayTimer,
                                 offer_delay = OfferDelayTimer}};

handle_event(cast, accept_reservations, open, Data) ->
    notify(Data#data.subscribers, accepting_reservations),
    {next_state, {accepting_reservations, {[], []}}, Data};

handle_event(cast, accept_offers, {accepting_reservations, {BuyResv, SellResv}}, Data)
  when length(BuyResv) > 0, length(SellResv) > 0 ->
    notify(Data#data.subscribers, market_formed),
    {next_state, {accepting_offers, {BuyResv, SellResv}}, Data};
handle_event(cast, accept_offers, {accepting_reservations, _}, Data) ->
    notify(Data#data.subscribers, market_error),
    {next_state, {market_done, {error, not_formed}}, Data};
handle_event({call, From}, {make_reservation, buyer}, {accepting_reservations, {BuyResv, SellResv}}, Data) ->
    ReservationId = erlang:make_ref(),
    {next_state, {accepting_reservations, {[ReservationId|BuyResv], SellResv}}, Data, [{reply, From, {ok, ReservationId}}]};
handle_event({call, From}, {make_reservation, seller}, {accepting_reservations, {BuyResv, SellResv}}, Data) ->
    ReservationId = erlang:make_ref(),
    {next_state, {accepting_reservations, {BuyResv, [ReservationId|SellResv]}}, Data, [{reply, From, {ok, ReservationId}}]};

handle_event({call, From}, {make_offer, Id, Offer}, {accepting_offers, {Buyers, Sellers}},
             #data{market_impl = MarketImpl} = Data) ->
    case {lists:member(Id, Buyers), lists:member(Id, Sellers)} of
        {true, _} ->
            gen_statem:reply(From, offer_accepted),
            NewData = add_buy_offer(Data, Offer),
            NewBuyers = Buyers -- [Id],
            NewSellers = Sellers;
        {_, true} ->
            gen_statem:reply(From, offer_accepted),
            NewData = add_sell_offer(Data, Offer),
            NewSellers = Sellers -- [Id],
            NewBuyers = Buyers;
        {false, false} ->
            gen_statem:reply(From, offer_rejected),
            NewData = Data,
            NewSellers = Sellers,
            NewBuyers = Buyers
    end,
    RemainingReservations = length(NewSellers) + length(NewBuyers),
    if RemainingReservations =:= 0 ->
            case MarketImpl:clear(NewData#data.buy_offers, NewData#data.sell_offers) of
                {ok, MCP} ->
                    notify(Data#data.subscribers, {market_cleared, MCP}),
                    {next_state, {market_done, cleared}, NewData#data{mcp = MCP}};
                {error, Reason} ->
                    notify(Data#data.subscribers, market_failed),
                    {next_state, {market_done, {error, Reason}}, NewData}
            end;
       RemainingReservations > 0 ->
            {next_state, {accepting_offers, {NewBuyers, NewSellers}}, NewData}
    end;

handle_event(cast, open_market, {market_done, _}, Data) ->
    {ReservationDelayTimer, OfferDelayTimer} = start_timers(Data#data.periodic),
    notify(Data#data.subscribers, market_open),
    {next_state, open, Data#data{buy_offers = [],
                                 sell_offers = [],
                                 mcp = undefined,
                                 offer_delay = OfferDelayTimer,
                                 reservation_delay = ReservationDelayTimer}};
handle_event(cast, open_market, State, Data) ->
    logger:error("market failed to clear in alloted time~nState = ~p", [State]),
    notify(Data#data.subscribers, market_failed),
    notify(Data#data.subscribers, market_open),
    {ReservationDelayTimer, OfferDelayTimer} = start_timers(Data#data.periodic),
    {next_state, open, Data#data{buy_offers = [],
                                 sell_offers = [],
                                 mcp = undefined,
                                 offer_delay = OfferDelayTimer,
                                 reservation_delay = ReservationDelayTimer}};

handle_event({call, From}, {make_reservation, _}, _, _) ->
    {keep_state_and_data, [{reply, From, {error, rejected}}]};
handle_event({call, From}, {make_offer, _, _}, _, _) ->
    {keep_state_and_data, [{reply, From, offer_rejected}]};

handle_event(cast, {subscribe, Pid}, _, Data) ->
    {keep_state, Data#data{subscribers = [Pid|Data#data.subscribers]}}.

add_buy_offer(#data{buy_offers = BuyOffers} = Data, Offer) ->
    Data#data{buy_offers = [Offer|BuyOffers]}.
add_sell_offer(#data{sell_offers = SellOffers} = Data, Offer) ->
    Data#data{sell_offers = [Offer|SellOffers]}.


notify(Pids, Msg) ->
    [Pid ! {gen_market, Msg} || Pid <- Pids].

start_timers(#{reservation_delay := ReservationDelay,
               offer_delay := OfferDelay}) ->
    {ok, ReservationDelayTimer} =
        timer:apply_after(ReservationDelay, gen_market, accept_reservations, [self()]),
    {ok, OfferDelayTimer} =
        timer:apply_after(OfferDelay, gen_market, accept_offers, [self()]),
    {ReservationDelayTimer, OfferDelayTimer}.
