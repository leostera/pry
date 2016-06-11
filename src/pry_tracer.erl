%%%-------------------------------------------------------------------
%% @doc tracing module in charge of capturing relevant traces
%% @end
%%%-------------------------------------------------------------------
-module(pry_tracer).

-export([
         start/0,
         stop/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec stop() -> ok.
stop() -> dbg:stop_clear().

-spec start() -> pid().
start() ->
  {ok, TracerPid} = dbg:tracer(process, options()),
  trace_all_processes(),
  trace_all_spawn_calls(),
  TracerPid.

%%====================================================================
%% Internal functions
%%====================================================================

-spec initial_trace_value() -> atom().
initial_trace_value() -> initializing.

-spec options() -> { fun(), term() }.
options() -> {fun filter/2, initial_trace_value()}.

trace_all_processes() -> dbg:p(all,call).

match_options() -> [{'_',[],[{return_trace}]}].

match_specs() -> [
                  {erlang, spawn, '_'},
                  {erlang, spawn_link, '_'},
                  {proc_lib, spawn, '_'},
                  {proc_lib, spawn_link, '_'},
                  {proc_lib, start, '_'},
                  {proc_lib, start_link, '_'}
                 ].

trace_all_spawn_calls() ->
  [ dbg:tpl( Spec, match_options() ) || Spec <- match_specs() ].

-spec filter(pry:trace_result(), ok | term()) -> ok.
filter({trace, Parent, return_from, N, {ok, Child}}=_Trace, ok) ->
  filter({trace, Parent, return_from, N, Child}, ok);
filter({trace, _Parent, return_from, _, Child}=Trace, ok) ->
  Timestamp = os:timestamp(),
  ProcessInfo = process_info(Child),
  case mfa_filter(ProcessInfo) of
    {ok, _}  ->
      Event = build_event(Trace, ProcessInfo, Timestamp),
      track(Event)
      %% setup link to know when it dies
      %% and when it dies, save an event as well
      ;
    {error, Error} ->
      Error
  end;
filter(_, _) -> ok.

-spec mfa_filter(pry:process_info()) -> undefined | blacklisted | pry:process_info().
mfa_filter(ProcessInfo) ->
  case pry_utils:get_mfa_from_process_info(ProcessInfo) of
    none   -> {error, no_initial_call};
    {M,_,_}-> case pry_blacklist:is_blacklisted(M) of
                true  -> {error, blacklisted};
                false -> {ok, ProcessInfo}
              end
  end.

-spec build_event(pry:trace_result(), pry:process_info(), pry:timestamp()) -> pry:event().
build_event({trace, Parent, return_from, _, Child}, ProcessInfo, Timestamp) ->
 #{
   timestamp => Timestamp,
   parent => Parent,
   self   => Child,
   mfa    => pry_utils:get_mfa_from_process_info(ProcessInfo),
   info   => ProcessInfo
  }.

-spec track(pry:event()) -> ok.
track(Event) ->
  gen_server:cast(pry_server:name(), {track, Event}).

