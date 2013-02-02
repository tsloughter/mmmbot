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

-record(state, {bucket="mmmbotimages", mp, mp_ssl}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start() ->
    mmmbot_em:add_handler(?SERVER).

%%--------------------------------------------------------------------
init([]) ->
    {ok, AccessKey} = application:get_env(mmmbot_images, access_key),
    {ok, SecretKey} = application:get_env(mmmbot_images, secret_key),    
    erlcloud_s3:configure(AccessKey, SecretKey),

    {ok, MP} = re:compile("http://(\\S*)(\\.jpg|\\.png|\\.gif|\\.jpeg|\\.xmp|\\.tiff)", [caseless]),
    {ok, MPSSL} = re:compile("https://(\\S*)(\\.jpg|\\.png|\\.gif|\\.jpeg|\\.xmp|\\.tiff)", [caseless]),

    State = case application:get_env(mmmbot_images, bucket)  of
                {ok, Bucket} ->
                    #state{bucket=Bucket, mp=MP, mp_ssl=MPSSL};
                _ ->
                    #state{}
            end, 

    {ok, State}.

%%--------------------------------------------------------------------
handle_event({Line, _User}, State=#state{bucket=Bucket, mp=MP, mp_ssl=MPSSL}) ->
    AWSConfig = erlcloud_aws:default_config(), 
    proc_lib:spawn_link(fun() -> 
                                parse(Bucket, Line, MP, MPSSL, AWSConfig)
                        end),
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

-spec parse(string(), string(), re:mp(), re:mp(), tuple()) -> ok.
parse(Bucket, Msg, MP, MPSSL, AWSConfig) -> 
    case re:run(Msg, MP, [{capture, first, list}]) of
        nomatch ->
            case re:run(Msg, MPSSL, [{capture, first, list}]) of
                nomatch ->
                    ok;
                {match, [URL]} ->
                    image_to_s3(Bucket, URL, true, AWSConfig)
            end;
        {match, [URL]} ->
            image_to_s3(Bucket, URL, false, AWSConfig)
    end.
       
-spec image_to_s3(string(), string(), boolean(), tuple()) -> proplists:proplist().
image_to_s3(Bucket, URL, IsSSL, AWSConfig) ->
    ExtStr = string:substr(URL, string:rchr(URL, $.)),
    Filename = generate_filename(string:sub_word(filename:basename(URL), 1, $.), ExtStr),

    {ok, "200", _, Image} = ibrowse:send_req(URL, [], get, "", 
                                             [{is_ssl, IsSSL}, {ssl_options, []}]),

    lager:info("Uploading image ~p~n", [Filename]),
    erlcloud_s3:put_object(Bucket, Filename, Image, AWSConfig).

-spec generate_filename(string(), string()) -> string().
generate_filename(Basename, ExtStr) ->
    lists:flatten([Basename, "_", integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:local_time())), ExtStr]).

%%%===================================================================
%%% Tests
%%%===================================================================
