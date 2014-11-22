%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Main logic for session server.
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

-record(state, {iodevice, filepos}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    Mem = start_inotify_and_mmap(),
    State = init_state(Mem),
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(test_mmap, State) ->
    Mem = get_state_iodevice(State),
    FilePos = get_state_filepos(State),
    {ok, Reply} = file:pread(Mem, FilePos, 4096 - FilePos),
    NewFilePos = FilePos + byte_size(Reply),
    io:format("~w ~w~n", [Reply, NewFilePos]),
    %NewState = set_state_filepos(State, NewFilePos),
    {noreply, State#state{filepos = NewFilePos}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Mem = get_state_iodevice(State),
    ok = file:close(Mem),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Inotify callbacks
%% ===================================================================

inotify_event(_Arg, _EventRef, _Msg = ?inotify_msg(_Mask = [?MODIFY], _Cookie, _Name)) ->
    gen_server:cast(?SERVER, test_mmap),
    ok;

inotify_event(_Arg, _EventRef, _Msg) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

start_inotify_and_mmap() ->
    application:start(inotify),
    {ok, Mem} = emmap:open(?MMAP_FILE, [read, shared, direct]),
    Ref = inotify:watch(?MMAP_FILE),
    inotify:print_events(Ref),
    inotify_evt:add_handler(Ref, ?MODULE, []),
    Mem.

init_state(IoDevice) ->
    State = #state{iodevice = IoDevice, filepos = 0},
    State.

get_state_iodevice(State) ->
    State#state.iodevice.

get_state_filepos(State) ->
    State#state.filepos.

set_state_filepos(State, fpos) ->
    State#state{filepos = fpos}.