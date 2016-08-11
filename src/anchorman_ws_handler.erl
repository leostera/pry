%%%-------------------------------------------------------------------
%% @doc event handler for publishing events through a websocket
%% @end
%%%-------------------------------------------------------------------

-module(anchorman_ws_handler).

-behaviour(gen_event).

%% gen_server callbacks
-export([
         init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

init(S0) ->
  {ok, S0}.

handle_info(_, State) -> {ok, State}.

handle_call(_, S) -> {ok, ok, S}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Handler functions
%%====================================================================

handle_event(Event, S) ->
  io:format("[anchorman_ws@handle_event] ~p", [Event]),
  {ok, S}.
