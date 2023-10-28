-module(byrslr).

-export([new_market/2, join_market/1, make_buy_reservation/1, make_sell_reservation/1, make_offer/3]).

%% @doc Start a new market.
-spec new_market(Id :: atom(), Options :: proplists:proplist()) -> ok | {error, market_exists}.
new_market(Id, Options) ->
    byrslr_manager:new_market(Id, Options).

%% @doc. Subscribe to notifications from `MarketId'.
join_market(MarketId) ->
    gen_market:join(MarketId).

%% @doc Make a reservation to buy in the market specified by
%% `MarketId'. The `Id' parameter is a unique identifier to be used
%% for the reservation and subsequent offer.
-spec make_buy_reservation(atom()) -> {ok, gen_market:reservation_id()} | {error, rejected}.
make_buy_reservation(MarketId) ->
    gen_market:make_reservation(MarketId, buyer).

%% @doc Make a reservation to sell in the market specified by
%% `MarketId'. The `Id' parameter is a unique identifier to be used
%% for the reservation and subsequent offer.
-spec make_sell_reservation(atom()) -> {ok, gen_market:reservation_id()} | {error, rejected}.
make_sell_reservation(MarketId) ->
    gen_market:make_reservation(MarketId, seller).

%% @doc Make an offer corresponding to the reservation `ReservationId'.
make_offer(MarketId, ReservationId, Offer) ->
    gen_market:make_offer(MarketId, ReservationId, Offer).
