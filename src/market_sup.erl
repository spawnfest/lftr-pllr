-module(market_sup).

-behaviour(supervisor).

-export([start_link/0, start_market/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_market(Id, Options) ->
    supervisor:start_child(?SERVER, [Id, Options]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 2,
                 period => 2000},
    ChildSpecs = [#{id => market,
                    start => {gen_market, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
