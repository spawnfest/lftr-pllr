%%%-------------------------------------------------------------------
%% @doc byrslr public API
%% @end
%%%-------------------------------------------------------------------

-module(byrslr_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    byrslr_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
