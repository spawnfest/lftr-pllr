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
[VOLTTRON](https://github.com/VOLTTRON/volttron).

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

## Build

    $ rebar3 compile
