-module(pry).

%%====================================================================
%% Public API
%%====================================================================

-export([
          dump/0,
          dump_to_file/1
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

-type server_state() :: #{
        table := atom(),
        tracer := pid()
       }.

-type trace_result() :: {trace, pid(), return_from, term(), pid()}.

-export_type([
              event/0,
              events/0,
              info/0,
              key/0,
              name/0,
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
