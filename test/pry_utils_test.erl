-module(pry_utils_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures, Constants and Helpers
%%====================================================================

fixture_mfa(none) -> fixture_mfa(proc_lib);
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

fixture_none_call() ->
  [ { initial_call, fixture_mfa(none) } ].


%%====================================================================
%% Tests
%%====================================================================

returns_none_for_none_calls_test() ->
  Fixture = fixture_none_call(),
  MFA = pry_utils:get_mfa_from_process_info(Fixture),
  ?assertEqual(none, MFA).

returns_initial_call_test() ->
  Fixture = fixture_process_with_initial_call(),
  MFA = pry_utils:get_mfa_from_process_info(Fixture),
  ?assertEqual(fixture_mfa(), MFA).

digs_into_dictionary_if_initial_call_is_proc_lib_init_p_test() ->
  Fixture = fixture_process_with_dictionary(),
  MFA = pry_utils:get_mfa_from_process_info(Fixture),
  ?assertEqual(fixture_mfa(), MFA).

