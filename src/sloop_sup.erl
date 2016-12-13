-module(sloop_sup).

-behaviour(supervisor).

-export([start_link/0, start_node/2, stop_node/1]).

%% Supervisor callbacks.
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

start_node(NodeName, ClusterMembers) ->
    % Define the FSM module supervision
    SupervisorName = generate_name(NodeName),
    start_child(SupervisorName, NodeName, ClusterMembers).

stop_node(NodeName) ->
    %% Brutal shutdown!!
    SupervisorName = generate_name(NodeName),
    ok = supervisor:terminate_child(?MODULE, SupervisorName),
    supervisor:delete_child(?MODULE, SupervisorName).

%% ================================================================================
%% Private
%% ================================================================================

start_child(SupervisorName, Name, Options) ->
    Sup = {SupervisorName,
           {sloop_node_sup, start_link, [Name, Options]},
           permanent, 5000, supervisor, [sloop_node_sup]},
    supervisor:start_child(?MODULE, Sup).

%% TODO crappy name, should be node_sup_name
generate_name(NodeName) ->
    list_to_atom(atom_to_list(NodeName) ++ "_sloop_node_sup").
