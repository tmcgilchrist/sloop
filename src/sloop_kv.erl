-module(sloop_kv).

-export([init/0, apply/1]).

apply({set, _Key, _Value}) ->
    %% TODO should store the key somewhere.
    ok;
apply({get, _Key}) ->
    %% TODO should retrieve the key from somewhere.
    {ok, <<"foo">>}.

init() ->
    %% Create ets table
    ok.
