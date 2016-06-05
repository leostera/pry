-module(pry_utils_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixtures, Constants and Helpers
%%====================================================================

fixture_process_with_initial_call() ->
  [ { initial_call, {pry, test, []} } ].

fixture_process_with_dictionary() ->
  [
    { initial_call, {proc_lib, init_p, 5} },
    { dictionary, [
                   {'$initial_call', {pry, test, []}}
                  ]
    }
  ].


%%====================================================================
%% Tests
%%====================================================================

returns_initial_call() ->
  Fixture = fixture_process_with_initial_call(),
  Module  = pry_utils:get_module_from_process_info(Fixture),
  ?assertEqual(pry, Module).

digs_into_dictionary_if_initial_call_is_proc_lib_init_p_test() ->
  Fixture = fixture_process_with_dictionary(),
  Module  = pry_utils:get_module_from_process_info(Fixture),
  ?assertEqual(pry, Module).

%%====================================================================
%% Test Helpers
%%====================================================================
