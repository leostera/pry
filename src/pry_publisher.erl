%%%-------------------------------------------------------------------
%% @doc pry top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pry_publisher).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
          name/0,
          start_link/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> ?MODULE.
name() -> ?MODULE.

-spec start_link() -> {'ok', pid()}.
start_link() ->
  gen_server:start_link({local, name()}, name(), [], []).

%%====================================================================
%% Internal functions
%%====================================================================

start_default_publisher() ->
  {ok, Server} = gen_event:start_link({local, pry_publisher_event_server}),
  ok = gen_event:add_sup_handler(Server, pry_publisher_handler, [#{ server => Server }]),
  Server.

-spec initial_state(list()) -> #{}.
initial_state(_Options) ->
  #{
    event_server => start_default_publisher()
   }.

%%====================================================================
%% Behavior functions
%%====================================================================

-spec init(list()) -> {ok, #{}}.
init(Options) ->
  State = initial_state(Options),
  {ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  pry_tracer:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast(Event, State) ->
  gen_event:notify(pry_publisher_event_server, Event),
  {noreply, State}.

handle_call(_Event, _From, State) ->
  {reply, not_implemeneted, State}.
