%%%-------------------------------------------------------------------
%% @doc websocket server
%% @end
%%%-------------------------------------------------------------------
-module(ws).

-behavior(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
         broadcast/1,
         name/0,
         start_link/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> ?MODULE.
name() -> ?MODULE.

broadcast(Message) -> gen_server:cast(name(), {broadcast, Message}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

-spec start_link( #{} ) -> {'ok', pid()}.
start_link(Options) ->
  gen_server:start_link({local, name()}, name(), Options, []).

-spec initial_state(list()) -> #{}.
initial_state(_Options) ->
  #{
   }.

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
%% Internal functions
%%====================================================================

publish(_, []) -> done;
publish(_Message, _Connection) -> ok.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast({broadcast, Message}, State) ->
  done = publish(Message, State),
  {noreply, State}.

handle_call(_, _, State) -> {noreply, State}.
