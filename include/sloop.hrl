%% State for the sloop_fsm server
-record(state, {
          leader :: term(),
          current_term :: non_neg_integer(),
          voted_for :: term(),
          timer :: timer:tref(),


          self         :: term(),         %% This node's id, registered name
          members = [] :: [pid()],        %% Pids of other cluster members


          candidate_id :: string(), %% Candidate Requesting Vote
          responses :: dict:dict()       %% Responses received from a vote request

}).

%% RPC Message formats

-record(request_vote, {
          term :: non_neg_integer(),
          candidate_id :: atom(),
          last_log_index :: non_neg_integer(),
          last_log_term :: non_neg_integer()
         }).

-record(vote, {
          id :: atom(),
          term :: non_neg_integer(),
          vote_granted :: boolean()
         }).


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
