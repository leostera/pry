%%%-------------------------------------------------------------------
%% @doc pry top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pry_publisher).

-behaviour(gen_event).

%% gen_server callbacks
-export([
         start_link/0,
         init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         loop/1,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_event:start_link({local, ?MODULE}).

init(_Args) ->
  {ok, Server} = evews_sup:start_link([{port, 2112}, {ws_handler, [{callback_m, ?MODULE}, {callback_f, loop}]}]),
  io:format("[ws@init] ~p", [Server]),
  {ok, #{ ws => Server }}.

handle_info(_, State) -> {ok, State}.

handle_call(_, S) -> {ok, ok, S}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================

parse(Term) -> jiffy:parse(Term).

loop({Ws, WsInfo}=State) ->
  receive
    Event ->
      io:format("[ws@loop] ~p", [Event]),
      Ws:send(Event, WsInfo),
      loop(State)
  end.

%%====================================================================
%% Handler functions
%%====================================================================

handle_event(Event, #{ ws:=WS }=S) ->
  io:format("[ws@handle_event] ~p", [Event]),
  WS ! parse(Event),
  {ok, S}.
