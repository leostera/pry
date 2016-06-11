%%%-------------------------------------------------------------------
%% @doc main server for spawning experiments
%% @end
%%%-------------------------------------------------------------------
-module(pry_server).

-behavior(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([name/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> ?MODULE.
name() -> ?MODULE.

-spec start_link() -> {'ok', pid()}.
start_link() ->
  gen_server:start_link({local, name()}, name(), [], []).

-spec initial_state() -> #{}.
initial_state() ->
  #{
    table => create_table(),
    tracer => pry_tracer:start()
  }.


-spec init(list()) -> {ok, #{}}.
init([]) ->
  State = initial_state(),
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

-spec table_name() -> atom().
table_name() -> pry_events.

-spec create_table() -> atom().
create_table() ->
  ets:new(table_name(), [ named_table ]).

-spec track(pry:event(), pry:server_state()) -> true.
track(#{ timestamp := Timestamp }=Event, #{ table := Table }) ->
  ets:insert(Table, {Timestamp, Event}).

-spec publish(pry:event(), pry:publishers()) -> done.
publish(_, []) -> done;
publish(Event, [ Publisher | Rest ]) ->
  Publisher ! Event,
  publish(Event, Rest);
publish(Event, #{ publishers := Publishers }) ->
  publish(Event, Publishers);
publish(_, _) -> done.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast({track, Event}, State) ->
  track(Event, State),
  publish(Event, State),
  {noreply, State}.

handle_call(dump, _From, #{ table := Table }=State) ->
  Reply = ets:tab2list(Table),
  {reply, Reply, State}.
