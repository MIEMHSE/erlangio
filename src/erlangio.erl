%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Main logic for ErlangIO.
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(erlangio).
-author("ssobko").

-behaviour(gen_server).
-behaviour(inotify).

%% API
-export([start_link/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

% Inotify callbacks
-export([inotify_event/3]).

%% Definitions
-include_lib("erlangio/include/erlangio.hrl").
-include_lib("inotify/include/inotify.hrl").
-define(SERVER, ?MODULE).

-record(state, {data}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    start_inotify(),
    State = init_state(),
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(get_data, State) ->
    Reply = raw_read_file(?ERLANGIO_PROC),
    io:format("Got reply: ~s~n", [binary_to_list(Reply)]),
    NewState = set_state_data(State, Reply),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Inotify callbacks
%% ===================================================================

inotify_event(_Arg, _EventRef, _Msg = ?inotify_msg(_Mask = [?CLOSE_WRITE], _Cookie, _Name)) ->
    gen_server:cast(?SERVER, get_data),
    ok;

inotify_event(_Arg, _EventRef, _Msg) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

start_inotify() ->
    application:start(inotify),
    Ref = inotify:watch(?ERLANGIO_DEV),
    inotify:print_events(Ref),
    inotify_evt:add_handler(Ref, ?MODULE, []).

init_state() ->
    State = #state{data = null},
    State.

set_state_data(State, Data) ->
    State#state{data = Data}.

raw_read_file(Path) ->
    {ok, File} = file:open(Path, [read, binary]),
    raw_read_loop(File, []).

raw_read_loop(File, Acc) ->
    case file:read(File, 5) of
        {ok, Bytes} ->
            raw_read_loop(File, [Acc | Bytes]);
        eof ->
            file:close(File),
            iolist_to_binary(Acc);
        {error, Reason} ->
            file:close(File),
            erlang:error(Reason)
    end.
