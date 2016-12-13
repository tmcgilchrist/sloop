%% State for the sloop_fsm server
-record(state, {
          leader :: term(),
          current_term :: non_neg_integer(),
          voted_for :: term(),
          timer :: timer:tref(),


          self         :: term(),         %% This node's id, registered name
          members = [] :: [pid()],        %% Pids of other cluster members

          log_entry :: log_entry(),       %% Current Log Entry being replicated
          candidate_id :: string(),       %% Candidate Requesting Vote
          responses :: dict:dict()        %% Responses received from a vote request or append_entries

}).
-type state() :: #state{}.

%% RPC Message formats

-record(request_vote, {
          term :: non_neg_integer(),
          candidate_id :: atom(),
          last_log_index :: non_neg_integer(),
          last_log_term :: non_neg_integer()
         }).
-type request_vote() :: #request_vote{}.

-record(vote, {
          id :: atom(),
          term :: non_neg_integer(),
          vote_granted :: boolean()
         }).

-type vote() :: #vote{}.

-record(append_entries, {
          term :: non_neg_integer(),
          leader_id :: atom(),
          prev_log_index :: non_neg_integer(),
          prev_log_term :: non_neg_integer(),
          entries,
          commit_index :: non_neg_integer()
         }).

-record(append_response, {
          id :: atom(),
          term :: non_neg_integer(),
          success :: boolean()
         }).

%% Operation to execute as part of the log
-record(op, {
          command :: any()
         }).

-record(log_entry, {
          index :: non_neg_integer(),
          term :: non_neg_integer(),
          command :: atom()}).

-define(LOG_ENTRY,  #log_entry).
-type log_entry() :: ?LOG_ENTRY{}.
-type log_entries() :: [log_entry()].
