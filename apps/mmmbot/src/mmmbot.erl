%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2012 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(mmmbot).

-behaviour(gen_server).

%% API
-export([start_link/0,
         send_message/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(help, ".help").
-define(check, ".check").

-record(state, {sock, channel, nickname}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_message(Msg) ->
    gen_server:cast(?SERVER, {message, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->    
    {ok, Host} = application:get_env(mmmbot, host),
    {ok, Port} = application:get_env(mmmbot, port),
    {ok, Nickname} = application:get_env(mmmbot, nickname),
    {ok, Channel} = application:get_env(mmmbot, channel),

    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
    gen_tcp:send(Sock, ["NICK ", Nickname, "\r\n"]),
    gen_tcp:send(Sock, ["USER ", Nickname, " blah blah blah blah\r\n"]),

    {ok, #state{sock=Sock, channel=Channel, nickname=Nickname}}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast({message, Msg}, State=#state{sock=Sock, channel=Channel}) ->
    irc_privmsg(Sock, Channel, Msg),
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->
    lager:info("[~w] Received: ~s", [Sock, Data]),
    parse_line(Sock, string:tokens(Data, ": "), Data, State),
    
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, #state{sock=Sock}) ->
    gen_tcp:close(Sock).

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Someone sent a message to us
parse_line(Sock, [User, "PRIVMSG", Channel, Nickname | _], _Msg, #state{nickname=Nickname}) ->
    Nick = lists:nth(1, string:tokens(User, "!")),
    irc_privmsg(Sock, Channel, [Nick, ": ", random_string()]);

% A message to the channel
parse_line(_, [User, "PRIVMSG", _Channel | _], Msg, _State) ->
    S1 = string:substr(Msg, string:rstr(Msg, " :")+2),
    S2 = string:substr(S1, 1, length(S1)-2),
    lager:info("Length: ~p, Message: ~p~n", [length(S1), S2]),

    %% Notify all listners of the message
    mmmbot_em:notify({S2, User});

% If the second token is "376", then join our channel.  376 indicates End of MOTD.
parse_line(Sock, [_, "376" | _], _Msg, #state{channel=Channel}) ->
    gen_tcp:send(Sock, ["JOIN :", Channel, "\r\n"]);

% The server will periodically send PINGs and expect you to PONG back to make sure
% you haven't lost the connection.
parse_line(Sock, ["PING"|Rest], _Msg, _State) ->
    gen_tcp:send(Sock, ["PONG ", Rest, "\r\n"]);

% Catch all
parse_line(_, _, _, _) ->
    ok.

% Random string
random_string() ->
    StringList = ["THAT'S NOT MY NAME",
                  "THEY CALL ME QUIET, BUT I'M A RIOT",
                  "MAJOR LAZER",
                  "STOP TALKIN' 'BOUT BLAH BLAH BLAH",
                  "FREE FREE PALESTINE",
                  "I LOVE THE WAY YOU RAP, BOOM, BOOM"],
   lists:nth(random:uniform(length(StringList)), StringList).

% This just helps us write a PRIVMSG back to a client without having to type
% the newlines and :'s ourselves so much.  It'll be more useful later.
irc_privmsg(Sock, To, Message) ->
    gen_tcp:send(Sock, ["PRIVMSG ", To, " :", Message, "\r\n"]).


