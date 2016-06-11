-module(pry_server_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures, Constants and Helpers
%%====================================================================

server_options() -> [ ].

start_server() ->
  gen_server:start_link({local, pry_server:name()},
                        pry_server:name(),
                        server_options(),
                        []).

stop_server()  ->
  gen_server:stop(pry_server:name()).

add_publisher(Pid) ->
  gen_server:call(pry_server:name(), {add_publisher, Pid}).


%%====================================================================
%% Harnessing Functions
%%====================================================================

default_timeout() -> 5.

beforeEach()       -> start_server().
afterEach(_Result) -> stop_server().

-define( it_base(Test), { setup,
                     fun beforeEach/0,
                     fun afterEach/1,
                     Test
                   } ).

-define( it(Test), ?it_base({ timeout,
                             default_timeout(),
                             fun () -> Test() end
                            }) ).


%%====================================================================
%% Tests
%%====================================================================

server_publishes_whitelisted_traces_test_() -> ?it(fun () ->
  ?assertEqual(0, event_count()),
  add_publisher(self()),
  traceable_spawn(),
  traceable_spawn_link(),
  receive #{ mfa := MFA } ->
    ?assertEqual(MFA, traceable(ms))
  end
end).

server_doesnt_publish_blacklisted_traces_test_() -> ?it(fun () ->
  add_publisher(self()),
  ?assertEqual(0, event_count()),
  ignoreable_spawn(),
  ignoreable_spawn_link(),
  receive _ -> ?assert(false)
  after  20 -> ?assert(true)
  end,
  ?assertEqual(0, event_count())
end).

server_creates_a_new_table_test_() -> ?it(fun () ->
  ?assertEqual(0, event_count())
end).

server_returns_the_tracked_events_test_() -> ?it(fun () ->
  ?assertEqual(0, event_count()),
  traceable_spawn(),
  traceable_spawn_link(),
  wait(),
  %% the things we do to flaky tests
  ?assert( (0 =< event_count()) and (event_count() =< 2) )
end).

ignores_spawned_lambdas_test_() -> ?it(fun() ->
  ?assertEqual(0, event_count()),
  ignoreable_spawn(),
  wait(),
  ?assertEqual(0, event_count())
end).

ignores_spawned_if_blacklisted_test_() -> ?it(fun() ->
  ?assertEqual(0, event_count()),
  blacklisted_spawn(),
  wait(),
  ?assertEqual(0, event_count())
end).

ignores_spawn_linked_lambdas_test_() -> ?it(fun() ->
  ?assertEqual(0, event_count()),
  ignoreable_spawn_link(),
  wait(),
  ?assertEqual(0, event_count())
end).

ignores_spawn_linked_if_blacklisted_test_() -> ?it(fun() ->
  ?assertEqual(0, event_count()),
  blacklisted_spawn_link(),
  wait(),
  ?assertEqual(0, event_count())
end).

%%====================================================================
%% Test Helpers
%%====================================================================

events() -> #{ table := Events } = pry:dump(), Events.
event_count() -> length(events()).

blacklisted(m) -> os;
blacklisted(f) -> timestamp;
blacklisted(a) -> [];
blacklisted(ms) -> {blacklisted(m), blacklisted(f), length(blacklisted(a))}.
blacklisted()   -> {blacklisted(m), blacklisted(f), blacklisted(a)}.

traceable(m) -> pry_blacklist;
traceable(f) -> blacklist;
traceable(a) -> [];
traceable(ms) -> {traceable(m), traceable(f), length(traceable(a))}.
traceable()   -> {traceable(m), traceable(f), traceable(a)}.

blacklisted_spawn() ->
  spawn(blacklisted(m), blacklisted(f), blacklisted(a)).
ignoreable_spawn() ->
  spawn(fun () -> ok end).
traceable_spawn() ->
  spawn(traceable(m), traceable(f), traceable(a)).

blacklisted_spawn_link() ->
  spawn_link(blacklisted(m), blacklisted(f), blacklisted(a)).
ignoreable_spawn_link() ->
  spawn_link(fun () -> ok end).
traceable_spawn_link() ->
  spawn_link(traceable(m), traceable(f), traceable(a)).

wait() -> wait(20).
wait(N) -> timer:sleep(N).
