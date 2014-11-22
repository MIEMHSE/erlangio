%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% ErlangIO application
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(erlangio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlangio_sup:start_link().

stop(_State) ->
    ok.
