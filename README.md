sloop
=====
[![Build Status](https://travis-ci.org/tmcgilchrist/sloop.svg?branch=master)](https://travis-ci.org/tmcgilchrist/sloop)
Documentation
=====

Run `rebar doc` and open doc/index.html to view the project documentation.

Compiling
======

  make get-deps
  make

Developing
=======
sloop uses `rebar3` to compile, so it needs to be available on your path.


``` shell
# Compiling
rebar3 compile
```

``` shell
# Running
rebar3 shell
```


TODO
====

 - rename message records to match the original paper more closely
 - leader log replication (*)
   - spec out the api for sloop_log
   - default implementation using in memory storage
   - sending / receiving replicate RPC messages
 - eunit/ct tests on fsm for leader election
   - leader election from startup
   - leader election from network partition or server failure
 - refactor out persistent state into it's own module
 - investigate example state machines and how they'll hook into RAFT
 - property based testing with properl or ??
 - cluster reconfiguration
 - log compaction

Components
=====

sloop_fsm = implements the consistency protocol

sloop_state = wrap the log store, under the covers I'll use ets for simplicity
but it could really be any perisitent store.

sloop_kv, sloop_echo = implementations of the state machine

sloop_config = handle dynamic reconfiguration of the RAFT cluster. (Not yet)

sloop_log = persistent store of the commands in the log
