%%%-------------------------------------------------------------------
%% @doc pry top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pry_publisher).

-behaviour(gen_event).

%% gen_server callbacks
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         loop/1,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

init(_Args) ->
  {ok, Server} = evews_sup:start_link([{port, 2112}, {ws_handler, [{callback_m, ?MODULE}, {callback_f, loop}]}]),
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
      Ws:send(Event, WsInfo),
      loop(State)
  end.

%%====================================================================
%% Handler functions
%%====================================================================

handle_event(Event, #{ ws:=WS }=S) ->
  WS ! parse(Event),
  {ok, S}.
