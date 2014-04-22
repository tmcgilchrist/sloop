sloop
=====

RAFT implementation in Erlang.

RAFT is an algorithm for achieving distributed consensus. It focuses on
providing the same guarantees as PAXOS but in a more understandable and simplier
to implement form. For more detailed information on RAFT go to <http://raftconsensus.github.io>

state_name/2 function is for async calls, state_name/3 is for sync calls.

Documentation
=====

Run `rebar doc` and open doc/index.html to view the project documentation.

Startup
=======

1. Run the shell with reloader. `make shell`
2. Start the sloop application. `application:start(sloop).`
3. Add nodes to the cluster. `sloop:start_node(node1, []).`

TODO
=====

gen_fsm (sloop_fsm) = implements the consistency protocol

gen_server (sloop_state) = wrap the log store, under the covers I'll use ets for simplicity
             but it could really be any perisitent store.

sloop_kv, sloop_echo = implementations of the state machine

sloop_config = handle dynamic reconfiguration of the RAFT cluster. (Not yet)

sloop_log = persistent store of the commands in the log

sloop_fsm
 - 3 states, follower, candidate, leader.


Current State:
- finish implementation of the leader election
  - defined vote/request_vote messages
  - write sending messages to other nodes
- write module to store log entries.
