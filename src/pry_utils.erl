-module(pry_utils).

-export([
         default/3,
         find_mfa/2,
         get_mfa_from_process_info/1,
         mfa_to_map/1,
         pid_to_map/1,
         process_info_to_map/1,
         timestamp_to_integer/1
        ]).

-spec default(atom(), pry:info(), term()) -> term().
default(Key, Options, Default) when is_list(Options) ->
  case proplists:lookup(Key, Options) of
    none -> Default;
    {Key, Val} -> Val
  end.

find_mfa({'$initial_call', MFA}, Info) ->
  find_mfa(MFA, Info);
find_mfa({dictionary, Dict}, Info) ->
  find_mfa(proplists:lookup('$initial_call', Dict), Info);
find_mfa({initial_call, {proc_lib, init_p, 5}}, Info) ->
  find_mfa(proplists:lookup(dictionary, Info), Info);
find_mfa({initial_call, MFA}, Info) ->
  find_mfa(MFA, Info);

find_mfa({supervisor, Module, 1}, _) -> {Module, start_link, 0};
find_mfa({_M,_F,_A}=MFA, _) -> MFA;
find_mfa(_, _) -> none.

-spec get_mfa_from_process_info(pry:info()) -> none | mfa().
get_mfa_from_process_info(ProcessInfo) when is_list(ProcessInfo) ->
  find_mfa(proplists:lookup(initial_call, ProcessInfo), ProcessInfo);
get_mfa_from_process_info(_) -> none.

process_info_to_map(Info) when is_list(Info) ->
  process_info_to_map(maps:from_list(Info));
process_info_to_map(#{
                       current_function := CF,
                       dictionary := Dict,
                       initial_call := IC,
                       links := Links
                     }=Info) ->
  Info#{
    current_function => mfa_to_map(CF),
    dictionary => maps:from_list(Dict),
    initial_call => mfa_to_map(IC),
    links => [ pid_to_map(Pid) || Pid <- Links ]
  }.

pid_to_map(Port) when is_port(Port) -> erlang:port_to_list(Port);
pid_to_map(Name) when is_atom(Name) ->
  pid_to_map(whereis(Name));
pid_to_map(Pid) when is_pid(Pid) ->
  % from <a.b.c> to a.b.c
  Body = lists:reverse(tl(lists:reverse(tl(pid_to_list(Pid))))),
  [ Node, Index, Wrap ] = string:tokens(Body, "."),
  pid_to_map({string:to_integer(Node),
             string:to_integer(Index),
             string:to_integer(Wrap)});
pid_to_map({{Node, _}, {Index, _}, {Wrap, _}}) ->
  #{
    node  => Node,
    index => Index,
    wrap  => Wrap
   }.

mfa_to_map({M, F, A}) ->
  #{
    module => M,
    function => F,
    arity => A
   }.

timestamp_to_integer({T0, T1, T2}) ->
  {Int, _} = string:to_integer(string:join(io_lib:format("~p~p~p", [T0,T1,T2]),"")),
  Int.
