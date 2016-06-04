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

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

initial_state() ->
  #{
    table => create_table(),
    tracer => start_tracer(),
    trace_specs => [
                    trace_all_processes(),
                    trace_all_spawn_calls()
                   ]
  }.


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

table_name() -> pry_events.

create_table() ->
  ets:new(table_name(), [ named_table ]).

initial_trace_value() -> initializing.

tracer_options() -> {fun tracer_filter/2, initial_trace_value()}.

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

track(Event) ->
  gen_server:cast(?MODULE, {track, Event}).

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
