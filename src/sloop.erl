-module(sloop).

-export([start_node/2,
         stop/0,
         set_config/2,
         op/2]).


%% Basically this is a test api into Sloop.
%% It provides helpful functions for probing and setting up a test cluster of Raft nodes.

start_node(Name, ClusterMembers) ->
    application:start(sloop),
    sloop_sup:start_node(Name, ClusterMembers).

stop() ->
    ok.

set_config(node1, _OtherNodes) ->
    %% Tells this node about other nodes that exist in the cluster.
    ok.

op(node1, _Command) ->
    %% Executes this command against node1.
    %% Return types,
    ok,  %% Command was executed successfullly
    _ = {error, <<"something is wrong">>}, %% Some unforseen error occured, not sure what this would be presently.
    {leader, node2}. %% Node1 isn't the leader, talk to this node instead.
