%% Simplest possible implementation of a RAFT state machine.
%% We export apply/1 which takes an erlang term, that is specific to this state machine.
%%

%% apply_term: anything, we just echo it out to stdout.

-module(sloop_echo).

-export([apply/1]).

apply(Command) ->
    io:format("~s~n", [Command]).
