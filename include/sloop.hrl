-record(state, {
          leader :: term(),
          current_term :: non_neg_integer(),
          voted_for :: term(),
          timer :: timer:tref(),

          candidate_id :: string(),

          members :: [pid()]      %% Pids of other cluster members
}).
