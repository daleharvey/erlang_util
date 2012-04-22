%% @doc Simple Jabber Client
%% 

-module(dh_jabber).
-author("Dale Harvey <dale@arandomurl.com>").

-export([ listen/2 ]).
-export([ do_listen/4 ]).
-export([ start/1 ]).

-export([ msg_presence/1 ]).
-export([ msg_muc/2 ]).

-import(string, [ tokens/2 ]).

-define(STR(Format, Args), lists:flatten(io_lib:format(Format, Args))).
-define(OPT,[list, {packet, 0}, {reuseaddr, true}]).

%%
%% EXPORTS
%%

start(Pid) ->
    {ok, spawn(fun() -> listen(null, Pid) end)}.

%% @doc 
connect(Host, Port, UserName, Password) ->

    application:start(crypto),

    F = fun() ->
                {ok, Socket} = gen_tcp:connect(Host, Port, ?OPT),
                do_listen(Socket, [], [], [])
        end,

    Master = spawn(F),

    ok = send(Master, msg_connect(Host)),
    {ok, _Stream}   = recv(Master),
    {ok, _Features} = recv(Master),
    
    ok = send(Master, msg_auth()),
    {ok, Challenge} = recv(Master),
    
    {challenge,_Attr,[AuthBin]} = simplexml_read_string(Challenge),    
    AuthStr = base64:decode_to_string(AuthBin),
    Split   = fun(X) -> [H,T] = tokens(X,"="), {H,string:strip(T,both,$")} end, %")} end,
    Auth    = lists:map(Split, tokens(AuthStr,",")),
    
    {value,{"nonce",Nonce}}  = lists:keysearch("nonce",1, Auth),
    CNonce = "1989181958",    
    NC = "00000001",

    Sasl = sasl_response(UserName, Password, Host, "xmpp/"++Host, 
                         Nonce, CNonce, NC),
    
    Resp = ["username=\"", UserName, "\",realm=\"", Host, "\",nonce=\"", 
            Nonce,"\",cnonce=\"", CNonce, "\",nc=", NC, ",qop=auth,",
            "digest-uri=\"xmpp/", Host, "\",response=\"", Sasl,
            "\",charset=\"utf-8\""], 
   
    AuthRsp = base64:encode_to_string(lists:flatten(Resp)),
    
    ok = send(Master, msg_response(AuthRsp)),
    
    {ok, _Challenge2} = recv(Master),
    
    ok = send(Master, "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>"),

    {ok, _Success} = recv(Master),

    ok = send(Master, msg_stream(Host)),

    {ok, _Stream2} = recv(Master),
    {ok, _Features2} = recv(Master),

    ok = send(Master, "<iq id='bind_1' type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></iq>"),

    {ok, _Bind} = recv(Master),

    ok = send(Master, msg_session(Host)),

    {ok, _Session} = recv(Master),

    {ok, Master}.

listen(Master, Pid) ->
    receive
        {connect, {Host, Port, UserName, Password}} ->
            {ok, NMaster} = connect(Host, Port, UserName, Password),
            NMaster ! {get_msg, self()},
            listen(NMaster, Pid);
        
        {ctl, close} ->
            Master ! {ctl, close};
        
        {send, Msg} ->
            Master ! {send, Msg},
            listen(Master, Pid);
        
        {msg, Msg} ->
            Pid ! {msg,Msg},
            Master ! {get_msg, self()},
            listen(Master, Pid)
    end.
           

%%
%% LOCAL FUNCTIONS
%%

send(Master, Data) ->
    Master ! {send, Data},
    ok.

recv(Master) ->
    Master ! {get_msg, self()},
    receive 
        {msg, Data} ->
            {ok, Data}
    after 
        10000 -> 
            timed_out
    end.

%% @doc router is waiting for a message, send
do_listen(Sock, [Msg|Rest], Fragment, [To|Wait]) ->
    To ! {msg, Msg},
    do_listen(Sock, Rest, Fragment, Wait);

%% @doc Main tcp control loop
do_listen(Sock, Msgs, Fragment, Waiting) ->
    receive 
        {ctl, close} ->
            ok = gen_tcp:close(Sock);

        {send, Msg} ->
            ok = gen_tcp:send(Sock, Msg),
            ?MODULE:do_listen(Sock, Msgs, Fragment, Waiting);

        {tcp, Sock, Msg} ->
            {NMsgs, Frag} = frame(Fragment ++ Msg),
            ?MODULE:do_listen(Sock, Msgs ++ NMsgs, Frag, Waiting);

        {get_msg,Pid} ->
            ?MODULE:do_listen(Sock, Msgs, Fragment, Waiting ++ [Pid])
    end.
 
%% @doc Split into a list of full messages and fragments
%% TODO : heh, lots
frame(Str) ->
    frame(Str,[]).

frame("<presence"++Rest, Acc) ->
    case string:str(Rest, "</presence>") of
        0 ->
            {[],"<presence"++Rest};
        X ->
            Msg  = "<presence"++string:substr(Rest, 1, X+10),
            Left = string:substr(Rest, X+11),
            frame(Left, Acc++[Msg])
    end;

frame("<message"++Rest, Acc) ->
    case string:str(Rest, "</message>") of
        0 ->
            {[],"<message"++Rest};
        X ->
            Msg  = "<message"++string:substr(Rest, 1, X+9),
            Left = string:substr(Rest, X+10),
            frame(Left, Acc++[Msg])
    end;

frame("<iq"++Rest, Acc) ->
    Pos  = string:str(Rest, "</iq>"),
    Msg  = "<iq"++string:substr(Rest, 1, Pos+4),
    Left = string:substr(Rest, Pos+5),
    frame(Left, Acc++[Msg]);

frame("<?xml version='1.0'?>"++Rest, Acc) ->
    {["<?xml version='1.0'?>"++Rest], Acc};

frame("<stream:features>"++Rest, Acc) ->
    {["<stream:features>"++Rest], Acc};

frame("<challenge"++Rest, Acc) ->
    {["<challenge"++Rest], Acc};

frame("<success"++Rest, Acc) ->
    {["<success"++Rest], Acc};

frame([], Acc) ->
    {Acc, []};

frame(Else, Acc) ->
    error_logger:info_msg("Else ~p~n", [Else]),
    {Acc, Else}.

sasl_response(User, Pass, Realm, Uri, Nonce, CNonce, NC) ->
    A1 = crypto:md5(?STR("~s:~s:~s",[User, Realm, Pass])),
    A2 = md5_hex(?STR("AUTHENTICATE:~s",[Uri])),
    HA1 = md5_hex(?STR("~s:~s:~s",[A1,Nonce,CNonce])),
    md5_hex(?STR("~s:~s:~s:~s:auth:~s",[HA1,Nonce,NC,CNonce,A2])).

simplexml_read_string(Str) ->
  Options = [{space,normalize},{encoding,"utf-8"}],
  {XML,_Rest} = xmerl_scan:string(Str,Options),
  xmerl_lib:simplify_element(XML).

md5_hex(S) ->
       Md5_bin =  erlang:md5(S),
       Md5_list = binary_to_list(Md5_bin),
       lists:flatten(list_to_hex(Md5_list)).
 
list_to_hex(L) ->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
%%
%% MESSAGE TEMPLATES
%%
msg_auth() ->
    ["<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' ",
    "mechanism='DIGEST-MD5'/>"].

msg_connect(To) ->
    ["<?xml version='1.0'?><stream:stream xmlns='jabber:client' ",
     "xmlns:stream='http://etherx.jabber.org/streams' to='",To,
     "' version='1.0'>"].

msg_response(Resp) ->
    ["<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>",Resp,
     "</response>"].

msg_stream(To) ->
    ["<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.",
     "jabber.org/streams' to='",To,"' version='1.0'>"].

msg_session(To) ->
    ["<iq to='",To,"' type='set' id='sess_1'><session xmlns='urn:",
     "ietf:params:xml:ns:xmpp-session'/></iq>"].

msg_presence(Room) ->
    ["<presence to='",Room,"'><x xmlns='http://jabber.org/protocol/muc'/>",
     "<history maxchars='0'/></presence>"].

msg_muc(Room,Text) ->
    ["<message type='groupchat' id='purple40527445' to='",Room,"'>"
     "<body>",Text,"</body><html xmlns='http://jabber.org/protocol/xhtml-im'>",
     "<body xmlns='http://www.w3.org/1999/xhtml'>yo</body></html></message>"].
