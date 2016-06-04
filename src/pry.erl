-module(pry).

%%====================================================================
%% Public API
%%====================================================================

-export([
          dump/0,
          test/0,
          test/1,
          dummy/1
         ]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type name() :: atom() | [char()] | <<>>.
-type timestamp() :: integer() | {integer(), integer(), integer()} | unset.

-type value() :: tuple().
-type key()   :: atom().
-type info()  :: [ {key(), value() } ].

-type event()  :: #{}.
-type events() :: [ {timestamp(), event()} ].

-export_type([
              name/0,
              timestamp/0,
              value/0,
              key/0,
              info/0,
              event/0,
              events/0
             ]).

%%====================================================================
%% API functions
%%====================================================================

-spec dump() -> events().
dump() ->
  gen_server:call(pry_server, dump).

-spec test() -> ok.
test() -> test(10).

-spec test(integer()) -> ok.
test(N) when N =< 0 -> ok;
test(N) ->
  erlang:spawn(fun () ->
                   erlang:spawn(pry, dummy, [N])
               end),
  test(N-1).

-spec dummy(integer()) -> true.
dummy(N) ->
  receive
  after N -> true
  end.
