%%%-------------------------------------------------------------------
%% @doc main server for spawning experiments
%% @end
%%%-------------------------------------------------------------------
-module(pry_server).

-include_lib("eunit/include/eunit.hrl").

-behavior(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([name/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec name() -> ?MODULE.
name() -> ?MODULE.

-spec start_link() -> {'ok', pid()}.
start_link() ->
  gen_server:start_link({local, name()}, name(), [], []).

-spec initial_state(list()) -> #{}.
initial_state(Options) ->
  #{
    table => create_table(),
    tracer => pry_tracer:start(),
    publishers => publishers(Options)
   }.

-spec init(list()) -> {ok, #{}}.
init(Options) ->
  State = initial_state(Options),
  {ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  pry_tracer:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec publishers(pry:info()) -> pry:publishers().
publishers(Options) ->
  [ pry_publisher | pry_utils:default(publishers, Options, []) ].

-spec table_name() -> atom().
table_name() -> pry_events.

-spec topic() -> anchorman:topic().
topic() -> <<"pry.event">>.

-spec create_table() -> atom().
create_table() ->
  ets:new(table_name(), [ named_table ]).

-spec track(pry:event(), atom()) -> true.
track(#{ timestamp := Timestamp }=Event, Table) ->
  ets:insert(Table, {Timestamp, Event}).

-spec publish(pry:event()) -> ok.
publish(Event) ->
  anchorman:broadcast(topic(), Event).

%%====================================================================
%% Handler functions
%%====================================================================

handle_cast({track, Event}, #{ table := Table }=State) ->
  true = track(Event, Table),
  ok = publish(Event),
  {noreply, State}.

handle_call(dump, _From, #{ table := Table }=State) ->
  Reply = State#{ table => ets:tab2list(Table) },
  {reply, Reply, State}.
