-module(sloop_app).

-behaviour(application).

-export([start/2, stop/1]).

%% Start of the real application, which has a supervisor and FSM module.

%% @doc application start callback for sloop
start(_Type, _StartArgs) ->
    sloop_sup:start_link().

%% @doc application stop callback for sloop
stop(_State) ->
    ok.
