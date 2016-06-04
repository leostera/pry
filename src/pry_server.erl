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

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {'ok', pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec initial_state() -> #{}.
initial_state() ->
  #{
    table => create_table(),
    tracer => start_tracer(),
    trace_specs => [
                    trace_all_processes(),
                    trace_all_spawn_calls()
                   ]
  }.


-spec init(list()) -> {ok, #{}}.
init([]) ->
  State = initial_state(),
  {ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
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

-spec initial_trace_value() -> atom().
initial_trace_value() -> initializing.

-spec tracer_options() -> { fun(), term() }.
tracer_options() -> {fun tracer_filter/2, initial_trace_value()}.

-spec start_tracer() -> pid().
start_tracer() ->
  {ok, TracerPid} = dbg:tracer(process, tracer_options()),
  TracerPid.

trace_all_processes() -> dbg:p(all,call).

tracer_match_options() -> [{'_',[],[{return_trace}]}].

tracer_match_spec() -> {erlang, spawn, '_'}.

trace_all_spawn_calls() ->
  dbg:tpl( tracer_match_spec(), tracer_match_options() ).

tracer_filter({trace, Parent, return_from, _, Child}, ok) ->
  ProcessInfo = process_info(Child),
  case ProcessInfo of
    undefined -> ok;
    [{current_function, MFA} | _Rest] ->
      Timestamp = os:timestamp(),
      Event = #{
        timestamp => Timestamp,
        parent => Parent,
        self   => Child,
        mfa    => MFA,
        info   => ProcessInfo
      },
      track(Event),
      publish(Event)
      %% setup link to know when it dies
      %% and when it dies, save an event as well
  end;
tracer_filter(_, _) -> ok.

-spec track(pry:event()) -> ok.
track(Event) ->
  gen_server:cast(?MODULE, {track, Event}).

-spec publish(pry:event()) -> ok.
publish(_) -> ok.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast({track, #{ timestamp := Timestamp }=Event}, #{ table := Table }=State) ->
  ets:insert(Table, {Timestamp, Event}),
  {noreply, State}.

handle_call(dump, _From, #{ table := Table }=State) ->
  Reply = ets:tab2list(Table),
  {reply, Reply, State}.
