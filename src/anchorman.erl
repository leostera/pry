-module(anchorman).

%%====================================================================
%% Public API
%%====================================================================

-export([
         broadcast/2,
         subscribe/2
        ]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type topic()   :: atom() | [char()] | binary().
-type message() :: term().
-type handler() :: atom().

%%====================================================================
%% API Functions
%%====================================================================

-spec broadcast( topic(), message() ) -> ok.
broadcast(Topic, Message) ->
  gen_server:cast(anchorman_server, {broadcast, Topic, Message}).

-spec subscribe( topic(), handler() ) -> ok.
subscribe(Topic, Handler) ->
  gen_server:call(anchorman_server, {suscribe, Topic, Handler}).
