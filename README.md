sloop
=====

RAFT implementation in Erlang.

RAFT is an algorithm for achieving distributed consensus. It focuses on
providing the same guarantees as PAXOS but in a more understandable and simplier
to implement form. For more detailed information on RAFT go to <http://raftconsensus.github.io>


TODO
=====

- start with node bring up
   1. what needs to be kept in the fsm state?

- keep nodes local within the VM initially, saves setting up tcp connections.

- state_name/2 function is for async calls, state_name/3 is for sync calls.
