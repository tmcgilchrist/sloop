-module(sloop_node_sup).

-behaviour(supervisor).

-export([ start_link/2
        , init/1
        ]).

start_link(Name, ClusterMembers) ->
    SupervisorName = list_to_atom(atom_to_list(Name) ++ "_sup"),
    start_link(Name, SupervisorName, Name, ClusterMembers).

init([A, Name, ClusterMembers]) ->

    %% Startup the log storage module
    Log = {sloop_log,
           {sloop_log, start_link, [A]},
           permanent, 5000, worker, [sloop_log]},

    %% Startup the sloop_fsm module, with the correct name
    Fsm = {sloop_fsm,
           {sloop_fsm, start_link, [fsm_name(A), Name, ClusterMembers]},
           permanent, 1, worker, [sloop_fsm]},

    {ok, {{one_for_all, 1, 1}, [Log, Fsm]}}.

start_link(A, SupName, Me, Opts) ->
    supervisor:start_link({local, SupName}, ?MODULE, [A, Me, Opts]).

fsm_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_fsm").
