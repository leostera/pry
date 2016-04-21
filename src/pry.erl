-module(pry).

-compile([export_all]).

start() ->
  redbug:start(trace_pattern(spawn), trace_options()),
  spawn(fun () ->
          receive
          after 2000 -> true
          end
        end).

trace_pattern(spawn) -> "erlang:spawn->return";
trace_pattern(spawn_link) -> "erlang:spawn_link->return".

trace_options() -> [
  {print_fun, fun track_process/2}
 ].

track_process(Msg, _Old) ->
  io:format("~p\n", [Msg]).

get_fun({_, {MFA, _}, _, _}) -> MFA;
get_fun(_) -> nope.
