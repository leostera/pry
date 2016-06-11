-module(pry).

%%====================================================================
%% Public API
%%====================================================================

-export([
          dump/0,
          dump_to_file/1,
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
-type info()  :: undefined | [ {key(), value()} ].

-type event()  :: #{}.
-type events() :: [ {timestamp(), event()} ].

-type publisher() :: pid().
-type publishers() :: [ publisher() ].

-type server_state() :: #{}.

-type trace_result() :: {trace, pid(), return_from, term(), pid()}.

-export_type([
              event/0,
              events/0,
              info/0,
              key/0,
              name/0,
              publisher/0,
              publishers/0,
              server_state/0,
              timestamp/0,
              trace_result/0,
              value/0
             ]).

%%====================================================================
%% API functions
%%====================================================================

-spec dump() -> events().
dump() ->
  gen_server:call(pry_server, dump).

-spec dump_to_file(<<>>) -> ok.
dump_to_file(Path) ->
  file:write_file(Path, io_lib:fwrite("~p.\n", [dump()])).

-spec test() -> ok.
test() -> test(10).

-spec test(integer()) -> ok.
test(N) when N =< 0 -> ok;
test(N) ->
  erlang:spawn(pry_blacklist, blacklist, []),
  test(N-1).

-spec dummy(integer()) -> true.
dummy(N) ->
  receive
  after N -> true
  end.
