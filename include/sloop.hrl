%% State for the sloop_fsm server
-record(state, {
          leader :: term(),
          current_term :: non_neg_integer(),
          voted_for :: term(),
          timer :: timer:tref(),


          self :: term(),           %% This node's id, registered name
          members :: [pid()],        %% Pids of other cluster members


          candidate_id :: string(), %% Candidate Requesting Vote
          responses :: dict()       %% Responses received from a vote request

}).


%% RPC Message formats

-record(request_vote, {
          term :: non_neg_integer(),
          candidate_id :: atom(),
          last_log_index :: non_neg_integer(),
          last_log_term :: non_neg_integer()}).

-record(vote, {
          term :: non_neg_integer(),
          vote_granted :: boolean()}).
