%%%-------------------------------------------------------------------
%% @doc pry top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(anchorman_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
          name/0,
          start_link/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> ?MODULE.
name() -> ?MODULE.

-spec event_server_name() -> atom().
event_server_name() -> anchorman_event_server.

-spec start_link( #{} ) -> {'ok', pid()}.
start_link(Options) ->
  gen_server:start_link({local, name()}, name(), [Options], []).

%%====================================================================
%% Internal functions
%%====================================================================

start_event_server() ->
  {ok, Server} = gen_event:start_link({local, event_server_name()}),
  Server.

start_handlers(_, []) -> ok;
start_handlers(EventServer, [Handler|Rest]) ->
  gen_event:add_sup_handler(EventServer, Handler, [ #{ server => EventServer } ]),
  start_handlers(EventServer, Rest).

-spec initial_state(#{}) -> #{}.
initial_state(Options) ->
  #{
    event_server => start_event_server(),
    handlers => maps:get(handlers, Options, [anchorman_ws_handler])
   }.

%%====================================================================
%% Behavior functions
%%====================================================================

-spec init(#{}) -> {ok, #{}}.
init(Options) ->
  #{
     event_server := ES,
     handlers := Handlers
   } = State = initial_state(Options),
  start_handlers(ES, Handlers),
  {ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #{ event_server := ES }=_State) ->
  gen_event:stop(ES),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast(Event, State) ->
  gen_event:notify(event_server_name(), Event),
  {noreply, State}.

handle_call(Event, From, State) ->
  gen_event:sync_notify(event_server_name(), {Event, From}),
  {noreply, State}.
