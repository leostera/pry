%%%-------------------------------------------------------------------
%% @doc main server for spawning experiments
%% @end
%%%-------------------------------------------------------------------
-module(pry_server).

-behavior(gen_server).

-compile([export_all]).

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
  stop_tracer(),
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

-spec stop_tracer() -> ok.
stop_tracer() ->
  dbg:stop_clear().

-spec start_tracer() -> pid().
start_tracer() ->
  {ok, TracerPid} = dbg:tracer(process, tracer_options()),
  TracerPid.

trace_all_processes() -> dbg:p(all,call).

tracer_match_options() -> [{'_',[],[{return_trace}]}].

tracer_match_specs() -> [
                         {erlang, spawn, '_'},
                         {erlang, spawn_link, '_'}
                        ].

trace_all_spawn_calls() ->
  [ dbg:tpl( Spec, tracer_match_options() ) || Spec <- tracer_match_specs() ].

-spec tracer_filter(pry:trace_result(), ok | term()) -> ok.
tracer_filter({trace, _Parent, return_from, _, Child}=Trace, ok) ->
  ProcessInfo = process_info(Child, [dictionary, initial_call, current_stacktrace, current_function, current_location]),
  case mfa_filter(ProcessInfo) of
    {ok, _} ->
      Timestamp = os:timestamp(),
      Event = build_event(Trace, ProcessInfo, Timestamp),
      track(Event),
      publish(Event)
      %% setup link to know when it dies
      %% and when it dies, save an event as well
      ;
    R -> R
  end;
tracer_filter(_, _) -> ok.

-spec mfa_filter(pry:info()) -> undefined | blacklisted | pry:process_dict().
mfa_filter(undefined) -> undefined;
mfa_filter(ProcessInfo) ->
  io:format("filtering: ~p\n\n", [ProcessInfo]),
  case get_mfa_from_process_dict(ProcessInfo) of
    {M, _, _} ->
      io:format("got module: ~p", [M]),
      case lists:member(M, pry_blacklist:blacklist()) of
        true -> blacklisted;
        false -> {ok, ProcessInfo}
      end;
    none -> ignored
  end.

get_mfa_from_process_dict(none) -> none;
get_mfa_from_process_dict({ '$initial_call', MFA }) -> MFA;
get_mfa_from_process_dict({ dictionary, Dict }) ->
  get_mfa_from_process_dict(proplists:lookup('$initial_call', Dict));
get_mfa_from_process_dict(Info) ->
  get_mfa_from_process_dict(proplists:lookup(dictionary, Info)).

-spec build_event(pry:trace_result(), pry:info(), pry:timestamp()) -> pry:event().
build_event({trace, Parent, return_from, _, Child}, ProcessInfo, Timestamp) ->
 #{
   timestamp => Timestamp,
   parent => Parent,
   self   => Child,
   mfa    => get_mfa_from_process_dict(ProcessInfo),
   info   => ProcessInfo
  }.

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
