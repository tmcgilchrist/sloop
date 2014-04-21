-module(sloop_log).

-behaviour(gen_server).

-record(state, {id :: atom()}).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link(Name) ->
    gen_server:start_link({local, logname(Name)}, ?MODULE, [Name], []).

init([Name]) ->
    {ok, #state{id=Name}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% =========================================================================================
%% Private
%% =========================================================================================

logname(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_log").
