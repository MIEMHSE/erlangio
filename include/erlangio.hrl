%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Common definitions for erlangio application
%%% @end
%%% Created : 18.10.2014 18:33
%%%-------------------------------------------------------------------
-author("ssobko").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Restart), {I, {I, start_link, []}, Restart, 5000, Type, [I]}).

%% Default values for supervisors
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%% Erlang IO Linux kernel device
-define(ERLANGIO_DEV, "/dev/erlangio").
-define(ERLANGIO_PROC, "/proc/erlangio").