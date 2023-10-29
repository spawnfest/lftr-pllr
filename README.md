byrslr
=====

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

To Do
-----

- [ ] implement the default market clearing algorithm.
- [ ] store market bids and clearing prices in mnesia
- [ ] configure as a distributed application and leverage mnesia for
      replication and failover

Build
-----

    $ rebar3 compile
