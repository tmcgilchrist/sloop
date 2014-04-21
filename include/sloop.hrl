-record(state, {
          leader :: term(),
          current_term :: non_neg_integer(),
          voted_for :: term(),
          timer :: timer:tref(),

          candidate_id :: string(),
          responses :: dict(),    %% Responses received from a vote request
          members :: [pid()]      %% Pids of other cluster members
}).
