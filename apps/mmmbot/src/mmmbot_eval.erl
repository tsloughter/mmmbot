%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2012 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(mmmbot_eval).

-behaviour(gen_event).

%% API
-export([start/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {funs=[]}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start() ->
    mmmbot_em:add_handler(?SERVER).

%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
handle_event({Line="fun() ->" ++ _, _User}, State=#state{funs=Funs}) ->
    {ok, Tokens, _} = erl_scan:string(Line),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {ok, State#state{funs=[Form | Funs]}};
handle_event({Line, User}, State=#state{funs=Funs}) ->
    lists:foreach(fun(Form) ->
                          Bindings =  erl_eval:add_binding('User', User, 
                                                           erl_eval:add_binding('Msg', Line, erl_eval:new_bindings())),
                          {value, Fun, _} = erl_eval:expr(Form, Bindings),   
                          Fun()
                  end, Funs),

    {ok, State}.



%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
