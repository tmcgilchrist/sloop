-module(sloop_log).

-behaviour(gen_server).

-record(log_entry, {
          index :: non_neg_integer(),
          term :: non_neg_integer(),
          command :: atom()}).

-define(LOG_ENTRY,  #log_entry).
-type log_entry() :: ?LOG_ENTRY{}.
%% -type log_entries() :: [log_entry()].

%% API
-export([ start_link/1
        , get_last_log_index/1
        , get_last_log_term/1
        , get_commit_index/1
        , push_entry/2
        , commit_entry/2
        , entry_at/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          id             :: term(),  %% Unique identifier for this logger, eg <node_name>_log

          dict           :: ordict:orddict(),
          last_log_entry :: #log_entry{},
          index = 0      :: non_neg_integer(),
          term = 0       :: non_neg_integer(),
          commit_index = 0 :: non_neg_integer()
}).

%% =========================================================================================
%% Public API
%% =========================================================================================

start_link(Name) ->
    gen_server:start_link({local, logname(Name)}, ?MODULE, [Name], []).

-spec(get_last_log_index(term()) -> non_neg_integer()).
get_last_log_index(Node) ->
    gen_server:call(Node, last_log_index).

-spec(get_last_log_term(term()) -> non_neg_integer()).
get_last_log_term(Node) ->
    gen_server:call(Node, last_log_term).

-spec(get_commit_index(term()) -> non_neg_integer()).
get_commit_index(Node) ->
    %% Commit index lags index slightly, waiting for a
    %% majority of responses from clients before being considered committed.
    gen_server:call(Node, commit_index).

-spec(push_entry(term(), log_entry()) -> ok | {error, string()}).
push_entry(Node, LogEntry) ->
    gen_server:call(Node, {push_entry, LogEntry}).

-spec(commit_entry(term(), log_entry()) -> {ok, non_neg_integer()} | {error, string()}).
commit_entry(Node, LogEntry) ->
    gen_server:call(Node, {commit_entry, LogEntry}).

entry_at(Node, Index, LogEntry) ->
    gen_server:call(Node, {entry_at, Index, LogEntry}).

%% =========================================================================================
%% gen_server Callbacks
%% =========================================================================================

init([Name]) ->
    Dict = orddict:new(),
    {ok, #state{id=Name, dict=Dict}}.

handle_call(last_log_index, _From, #state{index=Index}=State) ->
    {reply, Index, State};
handle_call(last_log_term, _From, #state{term=Term}=State) ->
    {reply, Term, State};
handle_call(commit_index, _From, #state{commit_index=CommitIndex}=State) ->
    {reply, CommitIndex, State};
handle_call({push_entry, LogEntry}, _From, #state{dict=Dict}=State) ->
    #log_entry{index=Index, term=Term} = LogEntry,
    NewDict = orddict:append(Index, LogEntry, Dict),
    NewState = State#state{dict=NewDict, index=Index, term=Term},
    {reply, ok, NewState};
handle_call({commit_entry, LogEntry}, _From, State) ->
    #log_entry{index=Index} = LogEntry,
    %% TODO Revist this and put in place extra checks for consistency
    NewState = State#state{commit_index=Index},
    {reply, {ok, Index}, NewState};
handle_call({entry_at, Index, LogEntry}, _From, #state{dict=Dict}=State) ->
    [A] = orddict:fetch(Index, Dict),
    case A of
        LogEntry ->
            {reply, true, State};
        _ ->
            {reply, false, State}
    end;
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
