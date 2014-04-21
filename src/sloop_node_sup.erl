-module(sloop_node_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(Name, ClusterMembers) when is_atom(Name) ->
    io:format("is_atom~n~n", []),
    SupervisorName = list_to_atom(atom_to_list(Name) ++ "_sup"),
    io:format("SupervisorName: ~p~n", [SupervisorName]),
    start_link(Name, SupervisorName, Name, ClusterMembers);
start_link(Name, ClusterMembers) ->
    io:format("other~n", []),
    {Me, _} = Name,
    SupervisorName = list_to_atom(atom_to_list(Name) ++ "_sup"),
    start_link(Me, SupervisorName, Name, ClusterMembers).


init(Args) ->
    %% Startup the log storage module
    io:format("sloop_node_sup:init/1 ~p~n", [Args]),
    [A, Name, ClusterMembers] = Args,
    %% Startup the sloop_fsm module, with the correct name
    Fsm = {sloop_fsm,
           {sloop_fsm, start_link, [A, Name, ClusterMembers]},
           permanent, 5000, worker, [sloop_fsm]},
    {ok, {{one_for_all, 5, 10}, [Fsm]}}.

start_link(A, SupName, Me, Opts) ->
    supervisor:start_link({local, SupName}, ?MODULE, [A, Me, Opts]).
