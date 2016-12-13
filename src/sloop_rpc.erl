-module(sloop_rpc).

-include("../include/sloop.hrl").

-export([send/2]).

send(NodeId, #append_entries{}=Msg) ->
    sloop_fsm:send(NodeId, Msg);
send(NodeId, #request_vote{candidate_id=From}=Msg) ->
    spawn(fun() ->
                  case sloop_fsm:send_sync(NodeId, Msg) of
                      Reply when is_record(Reply, vote) ->
                          sloop_fsm:send(From, Reply);
                      Error ->
                          %% TODO ignoring error here for now, watch as it bites me later.
                          lager:error("error sending: ~p~n", [Error])
                  end
          end).
