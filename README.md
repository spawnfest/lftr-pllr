# ByrSlr

An application for running double auction markets with customizable
auction clearing mechanisms.

Buy and sell offers can be arbitrary terms as long as you provide a
module implementing the `gen_market` behaviour to handle them. If you
don't want to implement your own market clearing algorithm `byrslr`
provides a default market mechanism that operates on `{Quantity,
Price}` tuples as offers.

The application supports periodic markets that restart at a fixed
interval, as well as purely event-driven markets. You can also run as
many concurrent markets as you want.

The architecture of the market is inspired by the market agent in
[VOLTTRON](https://github.com/VOLTTRON/volttron). Before making either
a buy or a sell offer, market participants must declare their intent
by making a buy/sell reservation. Reservations are assigned a unique
Id that must be used when making an offer. Once offers are received
corresponding to every reservation the market is automatically
cleared.

## Examples

### Creating an event driven market

To create an event-driven market with the default double-auction
clearing algorithm:

```erlang
byrslr:new_market(foo, []).
```
### Creating a periodic market

A market with a 5 minute cycle and a 120 second delay before accepting
offers.

```erlang
byrslr:new_market(foo, [{market_period, 300000}, {reservation_delay, 0}, {offer_delay, 120000}]).
```

### Using a custom clearing algorithm

Assuming the module `bar` implements the `gen_market` behavior, you
can specify that `bar` should be used to clear the market like so:

```erlang
byrslr:new_market(foo, [{market_impl, bar}]).
```

### Subscribe to market events

To subscribe to be notified when market events occur (e.g. starting to
accept reservations/offers, cleared, opened) use

```erlang
byrslr:join_market(foo).
```

Once subscribed the calling process will receive messages of the form
`{gen_market, Event}`.

### Driving an event-driven market

For event-driven markets use the following functions to trigger events
that drive the market.

```erlang
%% open the market, clearing previous bids and offers and starting a new bidding round
byrslr:start_new_cycle(foo).

%% these are pretty self-explanatory
byrslr:start_accepting_reservations(foo).
byrslr:start_accepting_offers(foo).
```

## Build

    $ rebar3 compile
