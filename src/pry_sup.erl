%%%-------------------------------------------------------------------
%% @doc pry top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  {ok, { supervision_flags(), child_specs() } }.

%%====================================================================
%% Internal functions
%%====================================================================

supervision_flags() -> #{
  strategy  => one_for_all,
  intensity => 0,
  peiord    => 1
 }.

child_spec(Module, Id) -> #{
  id       => Id,
  restart  => permanent,
  shutdown => brutal_kill,
  start    => { Module, start_link, [] },
  type     => worker
 }.

child_specs() -> [
                  child_spec(pry_server, pry),
                  child_spec(pry_publisher, pry_publisher)
                 ].
