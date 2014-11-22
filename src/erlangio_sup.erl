%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Main application supervisor.
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(erlangio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Definitions
-define(SERVER, ?MODULE).
-include_lib("erlangio/include/erlangio.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
    Spec = [
        ?CHILD(erlangio, worker, permanent)
    ],
    {ok, {Flags, Spec}}.
