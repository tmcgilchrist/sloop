%% Implements the consensus protocol via gen_fsm (state machine)

-module(sloop_fsm).

-behaviour(gen_fsm).

-include("sloop.hrl").

-export([filter/2]).


%% API
-export([start/1, start_link/1, start_link/3, send_sync/2, send/2]).

%% gen_fsm callbacks
-export([init/1, code_change/4
         , handle_event/3, handle_info/3
         , handle_sync_event/4, terminate/3]).

%% FSM state callbacks
-export([follower/2, follower/3
         , candidate/2, candidate/3
         , leader/2, leader/3
        ]).

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(A, Name, ClusterMembers) ->
    gen_fsm:start_link({local, A}, ?MODULE, [Name, ClusterMembers], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

send(To, Msg) ->
    gen_fsm:send_event(To, Msg).

send_sync(To, Msg) ->
    gen_fsm:sync_send_event(To, Msg, 100).

init(Name) ->
    [Id, ClusterMembers] = Name,
    Timer = gen_fsm:send_event_after(election_timeout(), timeout),
    NewState = #state{timer=Timer, self=Id, current_term=0, members=ClusterMembers},
    {ok, follower, NewState}.

handle_event(stop,_, State)->
    {stop, normal, State};

handle_event(_, _, State) ->
    io:format("handle_event~n", []),
    {stop, {error, badmsg}, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    io:format("handle_sync_event~n", []),
    {stop, badmsg, State}.

handle_info(_, StateName, State) ->
    %% Triggers state change
    NewState = State,  %% Update internal fsm state.
    {next_state, StateName, NewState}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_, _, _) ->
    ok.

%% =========================================================================================
%% States
%% =========================================================================================
follower(timeout, State) ->
    NewState = start_election(State),
    {next_state, candidate, NewState};

follower(Event, Data=#state{self=Id}) ->
    unexpected(Event, follower, Id),
    {next_state, follower, Data}.

follower(#request_vote{term=Term, candidate_id=_CandidateId}, _From, State=#state{current_term=CurrentTerm, self=Self}) ->
    case Term > CurrentTerm of
        true ->
            Vote = #vote{term=CurrentTerm, vote_granted=true, id=Self},
            io:format("~p #vote: ~p~n", [Self, Vote]),
            {reply, Vote, follower, State};
        false ->
            Vote = #vote{term=CurrentTerm, vote_granted=false, id=Self},
            io:format("~p #vote: ~p~n", [Self, Vote]),
            {reply, Vote, follower, State}
    end.

%% Election timeout has elapsed, start another election
candidate(timeout, State) ->
    NewState = start_election(State),
    {next_state, candidate, NewState};

candidate(V=#vote{id=From, term=_Term, vote_granted=VoteGranted},
          State=#state{responses=Responses, members=Members, self=Self}) ->
    io:format("~p candidate: vote: ~p~n", [Self, V]),
    % Record response
    R = dict:store(From, VoteGranted, Responses),

    % Check whether we've won the election
    case election_won(R, Members, Self) of
        % Either transition to leader or wait for more votes
        true ->
            %% TODO: Assert out leadership here to stop spurious timeouts and re-elections
            %% He who hesitates is lost.
            NewState = assert_leadership(State),
            {next_state, leader, NewState};
        false ->
            {next_state, candidate, State#state{responses=R}}
    end;

candidate(#append_entries{term=Term, leader_id=Leader}, State=#state{self=Id,timer=Timer}) ->
    io:format("~p stepping down for ~p term: ~p~n", [Id, Leader, Term]),
    gen_fsm:cancel_timer(Timer),            % Cancel timeouts while leader
    {next_state, follower, State};

candidate(Event, State=#state{self=Self}) ->
    io:format("~p candidate event: ~p data: ~p~n", [Self, Event, State]),
    {next_state, candidate, State}.

candidate(#request_vote{term=Term, candidate_id=CandidateId}, _From, State=#state{self=Self, current_term=CurrentTerm}) ->
    % Does it need to record who it voted for here? It should only vote once.
    case Term > CurrentTerm of
        true ->
            Vote = #vote{term=CurrentTerm, vote_granted=true, id=Self},
            sloop_fsm:send(fsm_name(CandidateId), Vote);
        false ->
            Vote = #vote{term=CurrentTerm, vote_granted=false, id=Self},
            sloop_fsm:send(fsm_name(CandidateId), Vote)
    end,
    {next_state, candidate, State}.


leader(timeout, State=#state{self=Id}) ->
    io:format("~p timeout State: ~p~n", [Id, State]),
    {next_state, leader, State};

leader(Event, State=#state{self=Id}) ->
    unexpected(Event, leader, Id),
    {next_state, leader, State}.

leader(#request_vote{term=Term, candidate_id=CandidateId}, _From, State=#state{current_term=CurrentTerm, self=Id}) ->
    case Term > CurrentTerm of
        true ->
            io:format("~p Stepping down as leader.~n", [Id]),
            {next_state, follower, State};
         false ->
            % Respond with current term and no_vote
            Vote = #vote{term=CurrentTerm, vote_granted=false, id=Id},
            sloop_fsm:send(fsm_name(CandidateId), Vote),
            {next_state, leader, State}
    end;

leader(Event, _From, State=#state{self=Id}) ->
    unexpected(Event, leader, Id),
    {next_state, leader, State}.

%%====================================
%% Private functions
%%====================================

election_won(Responses, Members, Self) ->
    Count = dict:size(dict:filter(fun(_, Vote) -> Vote end, Responses)),
    Votes = (length(Members) div 2 + 1),
    io:format("~p Members: ~p Responses: ~p Count: ~p Votes: ~p~n", [Self, Members, Responses, Count, Votes]),
    Count > (length(Members) div 2 + 1).

assert_leadership(State=#state{self=Id, members=Members, current_term=CurrentTerm, timer=Timer}) ->
    gen_fsm:cancel_timer(Timer),

    % Build no-op append_entries
    LastLogIndex = sloop_log:get_last_log_index(log_name(Id)),
    LastLogTerm = sloop_log:get_last_log_term(log_name(Id)),
    CommitIndex = sloop_log:get_commit_index(log_name(Id)),
    Msg = #append_entries{term=CurrentTerm,
                          leader_id=Id,
                          prev_log_index=LastLogIndex,
                          prev_log_term=LastLogTerm,
                          entries = [],
                          commit_index = CommitIndex},

    io:format("~p #append_entries{}: ~p members: ~p~n", [Id, Msg, filter(Id, Members)]),

    [sloop_rpc:send(fsm_name(N), Msg) || N <- filter(Id, Members)],

    State#state{responses=dict:new(), leader=Id}.

request_votes(#state{members=Members, self=Id, current_term=CurrentTerm}) ->
    LastLogIndex = sloop_log:get_last_log_index(log_name(Id)),
    LastLogTerm = sloop_log:get_last_log_term(log_name(Id)),

    VoteMsg = #request_vote{term=CurrentTerm, candidate_id=Id, last_log_index=LastLogIndex, last_log_term=LastLogTerm},

    io:format("~p #request_vote{}: ~p members: ~p~n", [Id, VoteMsg, Members]),
    [sloop_rpc:send(fsm_name(N), VoteMsg) || N <- filter(Id, Members)].

filter(_, []) ->
    [];
filter(Name, [Head|Tail]) ->
    case Head =:= Name of
        true ->
            filter(Name, Tail);
        false ->
            [Head|filter(Name, Tail)]
    end.

start_election(State = #state{current_term=CurrentTerm, self=Id}) ->
    io:format("~p Election timeout~p ~n", [Id, State]),
    Timer = reset_timer(),
    % increment current term, reset responses, clear leader
    R = dict:store(Id, true, dict:new()),
    NewState = State#state{current_term=CurrentTerm+1,
                  leader=undefined,
                  responses=R,
                  timer=Timer},
    io:format("~p, request_votes~n", [Id]),
    request_votes(NewState),
    NewState.

reset_timer() ->
    gen_fsm:send_event_after(election_timeout(), timeout).

election_timeout() ->
    crypto:rand_uniform(1500, 3000).

unexpected(Msg, State, Id) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [Id, Msg, State]).

fsm_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_fsm").

log_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_log").
