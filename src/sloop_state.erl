-module(sloop_state).

%% Persistent state module for saving,
%% - currentTerm: latest server term seen
%% - votedFor:    candidateId that receieved got in current term
%% - log[]:       log entries
