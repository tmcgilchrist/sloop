-module(sloop_test).

-compile(export_all).

main() ->
    {ok, Pid} = sloop_fsm:start_link("sloop_01", []),
    io:format("Spawned sloop_01: ~p~n", [Pid]),
    {ok, Pid2} = sloop_fsm:start_link("sloop_02", [Pid]),
    io:format("Spawned sloop_02: ~p~n", [Pid2]).
