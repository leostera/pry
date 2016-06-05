-module(pry_utils_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures, Constants and Helpers
%%====================================================================

fixture_mfa(blacklisted) -> fixture_mfa(proc_lib);
fixture_mfa(proc_lib) -> { proc_lib, init_p, 5 };
fixture_mfa(m) -> pry;
fixture_mfa(f) -> test;
fixture_mfa(a) -> [].
fixture_mfa() -> { fixture_mfa(m), fixture_mfa(f), fixture_mfa(a) }.

fixture_dictionary() -> { dictionary, [ {'$initial_call', fixture_mfa()} ] }.

fixture_process_with_initial_call() ->
  [ { initial_call, fixture_mfa() } ].

fixture_process_with_dictionary() ->
  [
    { initial_call, fixture_mfa(proc_lib) },
    fixture_dictionary()
  ].

fixture_blacklisted_call() ->
  [ { initial_call, fixture_mfa(blacklisted) } ].


%%====================================================================
%% Tests
%%====================================================================

returns_none_for_blacklisted_calls_test() ->
  Fixture = fixture_blacklisted_call(),
  Module  = pry_utils:get_module_from_process_info(Fixture),
  ?assertEqual(none, Module).

returns_initial_call_test() ->
  Fixture = fixture_process_with_initial_call(),
  Module  = pry_utils:get_module_from_process_info(Fixture),
  ?assertEqual(fixture_mfa(m), Module).

digs_into_dictionary_if_initial_call_is_proc_lib_init_p_test() ->
  Fixture = fixture_process_with_dictionary(),
  Module  = pry_utils:get_module_from_process_info(Fixture),
  ?assertEqual(fixture_mfa(m), Module).

%%====================================================================
%% Test Helpers
%%====================================================================
