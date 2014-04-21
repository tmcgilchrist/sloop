%% Implements the consensus protocol via gen_fsm (state machine)

-module(sloop_fsm).

-behaviour(gen_fsm).

-include("sloop.hrl").


%% API
-export([start/1, start_link/1, start_link/3]).

%% gen_fsm callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% FSM state callbacks
-export([follower/2, follower/3
         , candidate/2, candidate/3
         , leader/2, leader/3
        ]).

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(A, Name, ClusterMembers) ->
    io:format("sloop_fsm:start_link/3~n", []),
    gen_fsm:start_link({local, A}, ?MODULE, [Name, ClusterMembers], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

init(Name) ->
    io:format("init ~p~n", [Name]),
    [Id, _] = Name,
    %% Setup timeout for triggering election.
    Timer = gen_fsm:send_event_after(election_timeout(), timeout),
    %% Setup this module's state.
    NewState = #state{timer=Timer, self=Id, current_term=0},
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
    io:format("handle_info~n", []),
    %% Triggers state change
    NewState = State,  %% Update internal fsm state.
    {next_state, StateName, NewState}.

code_change(_OldVsn, StateName, State, _Extra) ->
    io:format("code_change~n", []),
    {ok, StateName, State}.

terminate(_, _, _) ->
    ok.

follower(timeout, #state{current_term=CurrentTerm, self=Id}=S0) ->
    io:format("Election timeout~p ~n", [S0]),
    reset_timer(election_timeout()),
    % increment current term, reset responses, clear leader
    S1 = S0#state{current_term=CurrentTerm+1,
                  leader=undefined,
                  responses=dict:new()},
    % request votes
    io:format("~p, request_votes~n", [Id]),
    request_votes(S1),
    % transition to candidate state
    {next_state, candidate, S1};
follower(Event, Data) ->
    unexpected(Event, follower),
    {next_state, follower, Data}.

follower(Event, _From, Data) ->
    unexpected(Event, follower),
    {next_state, follower, Data}.

% TODO if we receive an append_entry RPC with current_term higher or equal to ours, we
% step down as candidate. (Likely we timeout out reaching that node and they are
% the rightful leader).
candidate({request_vote_rpc, Term, VoteGranted}, Data) ->
    io:format("request_vote_rpc: ~p", [{request_vote_rpc, Term, VoteGranted}]),
    {next_state, candidate, Data};

%% Election timeout has elapsed, start another election
candidate(timeout, _State) ->
    io:format("election timeout, starting another election~n", []),
    {error, "Election timeout not handled yet!"};

% Catch all case, for debugging and raising WTF messages in the log
candidate(Event, Data) ->
    unexpected(Event, candidate),
    {next_state, candidate, Data}.

%% TODO: What's the difference between state/2 and state/3
candidate(Event, _From, Data) ->
    unexpected(Event, candidate),
    {next_state, candidate, Data}.

leader(Event, Data) ->
    unexpected(Event, leader),
    {next_state, leader, Data}.

leader(Event, _From, Data) ->
    unexpected(Event, leader),
    {next_state, leader, Data}.

%%====================================
%% Private functions
%%====================================

request_votes(#state{members=_Members, self=Id, current_term=CurrentTerm}) ->
    %% TODO Send out RequestVoteRPC to other nodes.
    %% TODO Where should last_log_term/index come from???
    VoteMsg = #request_vote{term=CurrentTerm, candidate_id=Id, last_log_index=0, last_log_term=0},

    % Send msg out to members using OTP, gen_fsm:send_event/2 which is an async send to each member
    %% [N!VoteMsg || N <- Members].
    io:format("CandidateId: ~p VoteMsg: ~p~n", [Id, VoteMsg]).

reset_timer(_Period) ->
    %% TODO implement timer for election timeouts.
    ok.

election_timeout() ->
    crypto:rand_uniform(150, 300).

unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, State]).
