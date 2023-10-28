-module(gen_market).

-behaviour(gen_statem).

-export([start_link/2]).
-export([callback_mode/0, init/1, handle_event/4]).

-record(data, {id :: atom(),
               module :: module(),
               state :: any()}).

open(Market) ->
    gen_statem:cast(Market, open).

start_link(Id, Arg) ->
    gen_statem:start_link(?MODULE, {Id, Arg}, []).

init({Id, Arg}) ->
    {ok, closed, #data{id = Id}}.

callback_mode() -> handle_event_function.

handle_event(cast, open, closed, Data) ->
    {next_state, accept_reservations, Data}.
