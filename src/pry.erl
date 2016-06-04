-module(pry).

-compile([export_all]).

initial_trace_value() -> initializing.

start() ->
  ets:new(pry_events, [ named_table ]),
  dbg:tracer(process, {fun tracer_filter/2, initial_trace_value()}),
  dbg:p(all,call),
  dbg:tpl(erlang,spawn,'_',[{'_',[],[{return_trace}]}]).

% tracer_filter(ThisCall, LastCallResult)
tracer_filter({trace, Parent, return_from, _, Child}, ok) ->
  ProcessInfo = process_info(Child),
  case ProcessInfo of
    undefined -> ok;
    [{current_function, MFA} | _Rest] ->
      Timestamp = timestamp(),
      Event = #{
        timestamp => Timestamp,
        parent => Parent,
        self   => Child,
        mfa    => MFA,
        info   => ProcessInfo
      },
      log("Child Spawned", Event),
      ets:insert(pry_events, {Timestamp, Event}),
      %% save into ets table
      %% publish creation on anchorman
      %% setup link to know when it dies
      %% publish death on anchorman
      log()
  end;
tracer_filter(_, _) -> ok.

dump() ->
  ets:tab2list(pry_events).

%%
%% Helper Functions
%%

log() -> io:format("\n\n").
log(Msg) ->
  io:format("~p:  ~p\n", [timestamp(), Msg]).
log(Prefix, Msg) ->
  io:format("~p - ~p: ~p\n", [timestamp(), Prefix, Msg]).

timestamp() -> os:timestamp().

test() -> start(), test(10).

test(N) when N =< 0 -> ok;
test(N) ->
  erlang:spawn(fun () ->
                   erlang:spawn(pry, dummy, [N])
               end),
  test(N-1).

dummy(N) ->
  receive
  after N -> true
  end.
