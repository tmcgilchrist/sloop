sloop
=====

Documentation
=====

Run `rebar doc` and open doc/index.html to view the project documentation.

Compiling
======

  make get-deps
  make

Developing
=======

1. Run the shell with reloader. `make shell`
2. Start the sloop test cluster of 3 nodes. `sloop:start_cluster().`


TODO
====

 - send periodic heart beat messages (DONE)
 - implement get_leader API function (DONE)
 - refactor timeout setup into common functions (DONE)
 -

Components
=====

sloop_fsm = implements the consistency protocol

sloop_state = wrap the log store, under the covers I'll use ets for simplicity
but it could really be any perisitent store.

sloop_kv, sloop_echo = implementations of the state machine

sloop_config = handle dynamic reconfiguration of the RAFT cluster. (Not yet)

sloop_log = persistent store of the commands in the log
