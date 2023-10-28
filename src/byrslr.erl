-module(byrslr).

-export([new_market/2]).

%% @doc Start a new market.
-spec new_market(Id :: atom(), Options :: proplists:proplist()) -> ok | {error, market_exists}.
new_market(Id, Options) ->
    byrslr_manager:new_market(Id, Options).
