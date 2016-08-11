-module(pry_utils_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures, Constants and Helpers
%%====================================================================

fixture_pid(n) -> 0;
fixture_pid(i) -> 1;
fixture_pid(w) -> 2.
fixture_pid() -> c:pid(fixture_pid(n), fixture_pid(i), fixture_pid(w)).

fixture_mfa(none) -> fixture_mfa(proc_lib);
fixture_mfa(proc_lib) -> { proc_lib, init_p, 5 };
fixture_mfa(m) -> pry;
fixture_mfa(f) -> test;
fixture_mfa(a) -> [].
fixture_mfa() -> { fixture_mfa(m), fixture_mfa(f), fixture_mfa(a) }.

fixture_dictionary(body) -> [ {'$initial_call', fixture_mfa()} ].
fixture_dictionary() -> { dictionary, fixture_dictionary(body) }.

fixture_process_with_initial_call() ->
  [ { initial_call, fixture_mfa() } ].

fixture_process_with_dictionary() ->
  [
    { initial_call, fixture_mfa(proc_lib) },
    fixture_dictionary()
  ].

fixture_none_call() ->
  [ { initial_call, fixture_mfa(none) } ].

fixture_full_process() ->
  [
   {current_function, fixture_mfa()},
   fixture_dictionary(),
   {initial_call, fixture_mfa()},
   {links, [fixture_pid()]}
  ].


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

process_info_to_map_test() ->
  Fixture = fixture_full_process(),
  #{
    current_function := CF,
    dictionary := Dict,
    initial_call := IC,
    links := Links
  }=pry_utils:process_info_to_map(Fixture),
  ?assertEqual(Links, [pry_utils:pid_to_map(fixture_pid())]),
  ?assertEqual(CF, pry_utils:mfa_to_map(fixture_mfa())),
  ?assertEqual(IC, pry_utils:mfa_to_map(fixture_mfa())),
  ?assertEqual(Dict, pry_utils:process_dict_to_map(fixture_dictionary(body))).

process_dict_to_map_test() ->
  Fixture = fixture_dictionary(body),
  #{
    '$initial_call' := IC
  }=pry_utils:process_dict_to_map(Fixture),
  ?assertEqual(IC, pry_utils:mfa_to_map(fixture_mfa())).

pid_to_map_test() ->
  Fixture = fixture_pid(),
  #{ node   := N,
     index  := I,
     wrap   := W
   } = pry_utils:pid_to_map(Fixture),
  ?assertEqual(fixture_pid(n), N),
  ?assertEqual(fixture_pid(i), I),
  ?assertEqual(fixture_pid(w), W).

mfa_to_map_test() ->
  Fixture = fixture_mfa(),
  #{ module   := M,
     function := F,
     arity    := A
   } = pry_utils:mfa_to_map(Fixture),
  ?assertEqual(fixture_mfa(m), M),
  ?assertEqual(fixture_mfa(f), F),
  ?assertEqual(fixture_mfa(a), A).

default_returns_value_if_key_is_present_test() ->
  ?assertEqual(val, pry_utils:default(key, [{key, val}], default)).

default_returns_fallback_if_key_is_not_present_test() ->
  ?assertEqual(default, pry_utils:default(key, [], default)).
