-module(gen_market_test).

-include_lib("eunit/include/eunit.hrl").

-define(MARKET, test_market).

make_reservation_test_() ->
    {setup, fun start_market/0, fun stop_market/1,
     {inorder,
      [fun reservation_rejected/0,
       fun open_market/0,
       fun reservation_rejected/0,
       fun start_accepting_reservations/0,
       fun sell_reservation_accepted/0,
       fun buy_reservation_accepted/0]}}.

start_market() ->
    {ok, Pid} = gen_market:start_link(?MARKET, []),
    Pid.

stop_market(Pid) ->
    unlink(Pid),
    gen_statem:stop(Pid).

reservation_rejected() ->
    {error, rejected} = gen_market:make_reservation(?MARKET, buyer).

open_market() ->
    gen_market:open_market(?MARKET).

start_accepting_reservations() ->
    gen_market:accept_reservations(?MARKET).

sell_reservation_accepted() ->
    {ok, _Ref} = gen_market:make_reservation(?MARKET, seller).

buy_reservation_accepted() ->
    {ok, _Ref} = gen_market:make_reservation(?MARKET, buyer).
