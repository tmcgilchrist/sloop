-record(log_entry, {
          index :: non_neg_integer(),
          term :: non_neg_integer(),
          command :: atom()}).

-define(LOG_ENTRY,  #log_entry).
-type log_entry() :: ?LOG_ENTRY{}.
-type log_entries() :: [log_entry()].
