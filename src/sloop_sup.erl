-module(sloop_sup).

-behaviour(supervisor).

-export([start_link/0, start_node/2, stop_node/1]).

%% Supervisor callbacks.
-export([init/1]).

%% 2 classes of functions
%% 1. the supervisor callbacks
%% 2. our own api

start_link() ->
    io:format("sloop_sup:start_link/0~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("sloop_sup:init/1~n", []),
    {ok, { {one_for_one, 5, 10}, []} }.

start_node(NodeName, ClusterMembers) ->
    io:format("sloop_sup:start_node/2 NodeName ~p Cluster members: ~p~n", [NodeName, ClusterMembers]),
    % Define the FSM module supervision
    SupervisorName = generate_name(NodeName),
    start_child(SupervisorName, NodeName, ClusterMembers).

stop_node(NodeName) ->
    %% Brutal shutdown!!
    ok = supervisor:terminate_child(?MODULE, NodeName),
    supervisor:delete_child(?MODULE, NodeName).

%% ================================================================================
%% Private
%% ================================================================================

start_child(SupervisorName, Name, Options) ->
    Sup = {SupervisorName,
           {sloop_node_sup, start_link, [Name, Options]},
           permanent, 5000, supervisor, [sloop_node_sup]},
    io:format("sloop_sup:start_child/3 ~p~n", [Sup]),
    supervisor:start_child(?MODULE, Sup).

generate_name(NodeName) ->
    list_to_atom(atom_to_list(NodeName) ++ "_sloop_node_sup").
