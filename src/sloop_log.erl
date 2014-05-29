-module(sloop_log).

-behaviour(gen_server).

-include("sloop_log.hrl").

%% API
-export([start_link/1, get_last_log_index/1, get_last_log_term/1, get_commit_index/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          id             :: term(),  %% Unique identifier for this logger, eg <node_name>_log

          %% TODO what storage engine are we going to use for this?

          last_log_entry :: #log_entry{},
          index = 0      :: non_neg_integer(),
          term = 0       :: non_neg_integer()
}).

%% =========================================================================================
%% Public API
%% =========================================================================================

start_link(Name) ->
    gen_server:start_link({local, logname(Name)}, ?MODULE, [Name], []).

get_last_log_index(Node) ->
    gen_server:call(Node, last_log_index).

get_last_log_term(Node) ->
    gen_server:call(Node, last_log_term).

get_commit_index(Node) ->
    % TODO Not sure what this should be at the moment
    gen_server:call(Node, last_log_index).


%% =========================================================================================
%% gen_server Callbacks
%% =========================================================================================

init([Name]) ->
    {ok, #state{id=Name}}.

handle_call(last_log_index, _From, #state{index=Index}=State) ->
    {reply, Index, State};
handle_call(last_log_term, _From, #state{term=Term}=State) ->
    {reply, Term, State};
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
