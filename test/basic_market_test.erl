-module(basic_market_test).

-include_lib("eunit/include/eunit.hrl").

clear_test() ->
    {ok, MCP} = byrslr_basic_market:clear([{2, 10}, {2, 8}, {2, 6}],
                                          [{1, 2}, {2, 4}, {2, 7}]),
    ?assertEqual(7.5, MCP).
