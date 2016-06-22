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
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

init(_Args) ->
  %% start web_server here
  ok.

handle_info(_, State) -> {ok, State}.

handle_call(_, S) -> {ok, ok, S}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Handler functions
%%====================================================================

handle_event(_Event, S) ->
  %JSON = jiffy:parse(Event),
  {ok, S}.
