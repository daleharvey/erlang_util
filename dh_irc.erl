%% @doc IRC Helper, 
%%
%% Connects to IRC Server and frames resulting message to 
%% relay them to a client.
-module(dh_irc).
-author("Dale Harvey <dale@arandomurl.com>").

%% Interface exports
-export([ start/1, close/1, connect/5, send/2 ]).
%% Internal exports (do not call)
-export([ listen/4 ]).

-define(OPTIONS,[list, {packet, 0}, {reuseaddr, true}]).

%%
%% EXPORTS
%%

-spec start(pid()) -> {ok, pid()}.
%% @doc Start the IRC Helper, Pid is where the message
%%      from irc will be forwarded
start(Pid) ->
    {ok, spawn(fun() -> listen(Pid, not_open, [], []) end)}.

-spec close(pid()) -> ok.
%% @doc Close connection to IRC (Pid is IRC Helper)
close(Pid) ->
    Pid ! close,
    ok.

-spec connect(pid(), string(), integer(), string(), string()) -> ok.
%% @doc Helper to connect to IRC, sending PASS, USER and NICK
connect(Pid, Host, Port, Name, Nick) ->
    Pid ! {connect, {Host, Port, Name, Nick}},
    ok.

-spec send(pid(), string()) -> ok.
%% @doc Send Message to IRC
send(Pid, Message) ->
    Pid ! {send, Message}.

%%
%% INTERNAL
%%

-spec listen(pid(), term(), list(), list()) -> ok.
%% @doc Main Control Loop

%% @doc Reply to PINGs automatically
listen(Pid, Sock, ["PING "++Msg | T ], Fragment) ->
    self() ! {send, "PONG "++Msg},
    listen(Pid, Sock, T, Fragment);

%% @doc Full Message in queue, send to client
listen(Pid, Sock, [Msg | T], Fragment) ->
    Pid ! {msg, Msg},
    listen(Pid, Sock, T, Fragment);

%% @doc Main control loop
listen(Pid, Sock, Msgs, Fragment) ->    
    receive 
        
        {connect, {Host, Port, Name, Nick}} ->
            
            {ok, Socket} = gen_tcp:connect(Host, Port, ?OPTIONS),
            self() ! {send, "PASS NOPASS\n"},
            self() ! {send, io_lib:format("NICK ~s\n",[Nick])},
            self() ! {send, io_lib:format("USER ~s 0 * :~s\n",[Name,Name])},
            ?MODULE:listen(Pid, Socket, Msgs, Fragment);
        
        close ->
            ok = gen_tcp:close(Sock);
        
        {send, Msg} ->
            ok = gen_tcp:send(Sock, Msg),
            ?MODULE:listen(Pid, Sock, Msgs, Fragment);
        
        {tcp, Sock, Msg} ->
            {NMsgs, Frag} = frame(Fragment ++ Msg),
            ?MODULE:listen(Pid, Sock, Msgs ++ NMsgs, Frag);
        
        _Else ->            
            error_logger:info_msg("Unhandled Message ~p ~n", [_Else]),
            ?MODULE:listen(Pid, Sock, Msgs, Fragment)
    end.

-spec frame(string()) -> {list(), string()}.
%% @doc Given a string, split it into \r\n delimited messages, store
%%      fragments seperately to be prepended to the next message
%% TODO : Guessing irc will allow escape \n's or something
frame(Str) ->
    case lists:reverse(Str) of
        [10|_Rest] ->
            {string:tokens(Str,"\r\n"), []};
        _  ->
            NMsgs = string:tokens(Str,"\r\n"),
            [Fragment|Rest] = lists:reverse(NMsgs),
            {lists:reverse(Rest), Fragment}
    end.
