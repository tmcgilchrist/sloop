-module(sloop).

-export([start_node/2,
         start_cluster/0,
         stop_node/1,
         stop/0,
         set_config/2,
         op/2,
         get_leader/1]).

-type command() :: any().

start_cluster() ->
    application:start(sloop),
    Nodes = [node1, node2, node3],
    [sloop:start_node(N, Nodes) || N <- Nodes].

%% Starts up a node with Name that knows about ClusterMembers
start_node(Name, ClusterMembers) ->
    sloop_sup:start_node(Name, ClusterMembers).

stop() ->
    Nodes = [node1, node2, node3],
    [stop_node(N) || N <- Nodes].

stop_node(Name) ->
    sloop_sup:stop_node(Name).

%% Tells this node about other nodes that exist in the cluster.
set_config(node1, _OtherNodes) ->
    ok.

%% Executes command against Node.
-spec(op(term(), command()) -> ok | not_leader | error).
op(Node, Command) ->
    case get_leader(Node) =:= Node of
        true ->
            sloop_fsm:op(fsm_name(Node), Command);
        false ->
            not_leader
    end.

%% Ask node who the leader is.
get_leader(Node) ->
    sloop_fsm:get_leader(fsm_name(Node)).

fsm_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_fsm").
