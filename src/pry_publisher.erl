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
  {ok, Server} = ws:start_link(ws_config()),
  {ok, #{ ws => Server }}.

handle_info(_, State) -> {ok, State}.

handle_call(_, S) -> {ok, ok, S}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================

ws_config() -> #{
 host => "localhost",
 port => "8080"
}.

parse(Term) -> jiffy:parse(Term).

%%====================================================================
%% Handler functions
%%====================================================================

handle_event(Event, S) ->
  ok = ws:broadcast(parse(Event)),
  {ok, S}.
