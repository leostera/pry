-module(pry).

-export([
          dump/0,
          test/0,
          test/1,
          dummy/1
         ]).

dump() ->
  gen_server:call(pry_server, dump).

test() -> test(10).

test(N) when N =< 0 -> ok;
test(N) ->
  erlang:spawn(fun () ->
                   erlang:spawn(pry, dummy, [N])
               end),
  test(N-1).

dummy(N) ->
  receive
  after N -> true
  end.
