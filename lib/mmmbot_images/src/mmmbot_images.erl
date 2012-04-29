%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2012 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(mmmbot_images).

-behaviour(gen_event).

%% API
-export([start/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {bucket="mmmbotimages"}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start() ->
    mmmbot_em:add_handler(?SERVER).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, AccessKey} = application:get_env(mmmbot_images, access_key),
    {ok, SecretKey} = application:get_env(mmmbot_images, secret_key),    
    erlcloud_s3:configure(AccessKey, SecretKey),

    State = case application:get_env(mmmbot_images, bucket)  of
                {ok, Bucket} ->
                    #state{bucket=Bucket};
                _ ->
                    #state{}
            end, 

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({Line, _User}, State=#state{bucket=Bucket}) ->
    parse(Bucket, Line),
    {ok, State#state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse(string(), string()) -> ok.
parse(Bucket, "http://" ++ _ = URL) -> 
    add_url(Bucket, URL, false);
parse(Bucket, "https://" ++  _ = URL) -> 
    add_url(Bucket, URL, true);
parse(_Bucket, _Msg) ->
    ok.

-spec add_url(string(), string(), boolean()) -> ok.
add_url(Bucket, URL, IsSSL) -> 
    io:format("Checking if image ~p:~p~n", [length(URL), URL]),
    ExtStr = string:substr(URL, string:rchr(URL, $.)),
    case is_image_ext(ExtStr) of
        true ->
            io:format("Is image~n"),
            image_to_s3(Bucket, URL, ExtStr, IsSSL),
            ok;
        _ ->
            io:format("Not an image~n"),
            ok
    end.

-spec image_to_s3(string(), string(), string(), boolean()) -> proplists:proplist().
image_to_s3(Bucket, URL, ExtStr, IsSSL) ->
    Filename = generate_filename(string:sub_word(filename:basename(URL), 1, $.), ExtStr),
    {ok, "200", _Headers, Image} = ibrowse:send_req(URL, [], get, "", [{is_ssl, IsSSL}, {ssl_options, []}]),
    erlcloud_s3:put_object(Bucket, Filename, Image).

-spec is_image_ext(string()) -> boolean().
% Compares to standard message extensions
is_image_ext(ExtStr) ->
    io:format("Checking string: ~p~n", [ExtStr]),
    lists:member(ExtStr, [".jpg", ".JPG", ".png", ".PNG", ".gif", ".GIF", 
                          ".jpeg", ".JPEG", ".xmp", ".XMP", ".tiff", ".TIFF"]).

-spec generate_filename(string(), string()) -> string().
generate_filename(Basename, ExtStr) ->
    lists:flatten([Basename, "_", integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:local_time())), ExtStr]).
