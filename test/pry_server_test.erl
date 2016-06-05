-module(pry_server_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures, Constants and Helpers
%%====================================================================

module_name() -> pry_server.
server_name() -> pry_server.

start_server() -> gen_server:start_link({local, server_name()}, module_name(), [], []).
stop_server()  -> gen_server:stop(server_name()).


%%====================================================================
%% Harnessing Functions
%%====================================================================

beforeEach() ->
  start_server().

afterEach(_Result) ->
  stop_server().

-define( it(Name), { setup,
                     fun beforeEach/0,
                     fun afterEach/1,
                     fun () -> Name() end } ).


%%====================================================================
%% Tests
%%====================================================================

server_creates_a_new_table_test_() -> ?it(fun () ->
  ?assertEqual(0, event_count())
end).

server_returns_the_tracked_events_test_() -> ?it(fun () ->
  ?assertEqual(0, event_count()),
  traceable_spawn(),
  ?assertEqual(1, event_count())
end).

ignores_spawned_lambdas_test_() -> ?it(fun() ->
  ignoreable_spawn(),
  ?assertEqual(0, event_count())
end).

ignores_spawned_if_blacklisted_test_() -> ?it(fun() ->
  blacklisted_spawn(),
  ?assertEqual(0, event_count())
end).

%%====================================================================
%% Test Helpers
%%====================================================================

events() -> pry:dump().
event_count() -> length(events()).

blacklisted_spawn() -> spawn_and_wait(os, timestamp, []).
ignoreable_spawn()  -> spawn_and_wait(fun () -> ok end).
traceable_spawn()   -> spawn_and_wait(pry_blacklist, blacklist, []).

spawn_and_wait(Fun) ->
  catch spawn(Fun),
  timer:sleep(2).
spawn_and_wait(M,F,A) ->
  catch spawn(M,F,A),
  timer:sleep(2).
