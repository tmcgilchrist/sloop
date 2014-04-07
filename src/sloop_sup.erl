-module(sloop_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Name) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Name]).

init([Name]) ->

    % Define the FSM module supervision
    Fsm = {sloop_fsm,
           {sloop_fsm, start_link, [Name]},
           permanent, 5000, worker, [sloop_fsm]},
    Processes = [Fsm],
    {ok, {{one_for_one, 10, 10}, Processes}}.
