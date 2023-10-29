-module(byrslr_basic_market).

-behaviour(gen_market).

-include_lib("eunit/include/eunit.hrl").

-export([clear/2]).

clear(BuyOffers, SellOffers) ->
    OrderedSellers = lists:keysort(2, SellOffers),
    OrderedBuyers = lists:reverse(lists:keysort(2, BuyOffers)),
    %% 1. Find the intersection of the ordered buyers & sellers
    case intersection(to_curve(OrderedBuyers, []),
                      to_curve(OrderedSellers, []))
    of
        no_intersection ->
            {error, no_intersection};
        Number ->
            {ok, Number}
    end.

intersection([{_, ZD}|_]=Demand, [{_, ZS}|_]=Supply) ->
    if ZD < ZS ->
            ?debugHere,
            no_intersection;
       true ->
            find_intersection(Demand, Supply, Supply, 0)
    end.

find_intersection([{Dx, Dy}]=Demand, [{Sx, Sy}]=Supply, _, PrevX) ->
    MarketVolume = find_volume(Demand, PrevX),
    Dv = value(Demand, MarketVolume, 0),
    Sv = value(Supply, MarketVolume, infinity),
    if Dv >= Sv ->
            (Dv + Sv) / 2;
       Dv < Sv ->
            D = value(Demand, PrevX, 0),
            S = value(Supply, PrevX, infinity),
            (D + S) / 2
    end;
find_intersection(Demand, Supply, [{X, _}|Rest], PrevX) ->
    case {value(Supply, X, infinity), value(Demand, X, 0)} of
        {Sx, Dx} when Sx < Dx ->
            find_intersection(Demand, Supply, Rest, X);
        {Sx, Dx} ->
            MarketVolume = find_volume(Demand, PrevX),
            Dv = value(Demand, MarketVolume, 0),
            Sv = value(Supply, MarketVolume, infinity),
            if Dv >= Sv ->
                    (Dv + Sv) / 2;
               Dv < Sv ->
                    D = value(Demand, PrevX, 0),
                    S = value(Supply, PrevX, infinity),
                    (D + S) / 2
            end
    end.

find_volume([{X, _}|Curve], MinX) when X >= MinX ->
    X;
find_volume([_|Curve], MinX) ->
    find_volume(Curve, MinX).

value(Curve, X, Default) ->
    value(Curve, X, 0, Default).

value([], _, _, Default) -> Default;
value([{XNext, Y}|Curve], X, XPrev, Default) ->
    if (X =< XNext) and (X >= XPrev) ->
            Y;
       true ->
            value(Curve, X, XNext, Default)
    end.

to_curve([], Curve) ->
    lists:reverse(Curve);
to_curve([{Quantity, Price}|Rest], []) ->
    to_curve(Rest, [{Quantity, Price}]);
to_curve([{Quantity, Price}|Rest], [{CumQuantity, _}|_] = Curve) ->
    to_curve(Rest, [{CumQuantity + Quantity, Price}|Curve]).
