-module(olifer_clock).

%% The current time for the date bounds "today", "yesterday" and "tomorrow" (LIVR 2.5).
%% A module of its own, so that tests can fix the time with meck.

-export([system_time/0]).

%% The system time in seconds since the Unix epoch, UTC
system_time() ->
    os:system_time(second).
