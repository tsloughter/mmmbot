%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2012 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(mmmbot_em).

%% API
-export([start_link/0, add_handler/1, notify/1]).

-define(SERVER, ?MODULE). 

%% Creates event manager
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% Add a handler
add_handler(Module) ->
    gen_event:add_handler(?SERVER, Module, []).

%% Notify all handlers of an event
notify(Event) ->
    gen_event:notify(?SERVER, Event).

