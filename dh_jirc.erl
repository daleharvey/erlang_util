%% @doc 
%% 

-module(dh_jirc).
-author("Dale Harvey <dale@arandomurl.com>").

-export([ start/0 ]).
-export([ bridge/2 ]).

-define(STR(Format, Args), lists:flatten(io_lib:format(Format, Args))).

start() ->

    Bridge = spawn(fun() -> bridge(null, null) end),

    IRC_Details = {"irc.freenode.net", 6667, "ecconrefused", "econnrefused-bot"},
    IRC_Room    = "##econnrefused",
    IRC_FSM = spawn(fun() -> irc(init, Bridge, IRC_Details, IRC_Room) end),
    
    XMPP_Details = {"econnrefused.com", 5222, "test", "test"},
    XMPP_Room    = "web@conference.econnrefused.com/^",
    XMPP_FSM = spawn(fun() -> xmpp(init, Bridge, XMPP_Details, XMPP_Room) end),

    Bridge ! {irc,  IRC_FSM},
    Bridge ! {xmpp, XMPP_FSM},
        
    {ok, Bridge}.

%% @doc IRC_FSM
%% TODO turn in to proper fsm
-define(JOINED, ":econnrefused-bot!n=ecconref@92.16.246.94 JOIN :##econnrefused").

irc(init, Bridge, Details, Room) ->

    {ok, IRC} = dh_irc:start(self()),

    IRC ! {connect, Details},
    IRC ! {send, "JOIN "++Room++"\n"},

    irc(joining, Bridge, IRC, []);

irc(joining, Bridge, IRC, Users) ->

    receive 
        {msg, ?JOINED} -> 
            irc(users, Bridge, IRC, []);
        {msg, _Msg} ->
            irc(joining, Bridge, IRC, Users)
    end;

irc(users, Bridge, IRC, Users) ->
    receive 
        {msg, Msg} ->
            case string:tokens(Msg, " ") of
                [_, _, _, _, ":End", "of", "/NAMES", "list."] ->
                    error_logger:info_msg("In IRC ~n", []),
                    irc(in_room, Bridge, IRC, Users);
                Bleh ->
                    error_logger:info_msg("add to users: ~p~n", [Bleh]),
                    irc(users, Bridge, IRC, Users)
                end
    end;

irc(in_room, Bridge, IRC, Users) ->
    receive 
        {ctl, close} ->
            IRC ! {ctl, close};

        {send, Msg} ->
            IRC ! {send, Msg},
            irc(in_room, Bridge, IRC, Users);

        {msg, M} ->
            case string:tokens(M," ") of
                [":"++Usr, "PRIVMSG", _Room | Rest] ->
                    [User|_Rest] = string:tokens(Usr, "!"),
                    Bridge ! {irc_msg, User, lists:flatten(Rest)},
                    irc(in_room, Bridge, IRC, Users);
                _Else ->
                    error_logger:info_msg("Groked ~p ~p~n", [_Else]),
                    irc(in_room, Bridge, IRC, Users)
            end
    end.


%% @doc XMPP_FSM
%% TODO turn in to proper fsm
xmpp(init, Bridge, Details, Room) ->

    {ok, XMPP} = dh_jabber:start(self()),

    XMPP ! {connect, Details},
    XMPP ! {send, dh_jabber:msg_presence(Room)},

    xmpp(joining, Bridge, XMPP, []);

xmpp(joining, Bridge, XMPP, _Users) ->

    Me = "web@conference.econnrefused.com/^",

    receive 
        {msg, Msg} -> 
            case sxml_read_string(Msg) of 

                {presence, [{from, Me}, {to, _To}], _Rest} ->
                    error_logger:info_msg("In Room! ~n", []),
                    xmpp(in_room, Bridge, XMPP, []);

                {presence, [{from, User} | _Dontcare], _Rest} ->                  
                    error_logger:info_msg("User: ~p~n", [User]),
                    xmpp(joining, Bridge, XMPP, [])
            end
    end;

xmpp(in_room, Bridge, XMPP, Users) ->
    receive 
        {ctl, close} ->
            XMPP ! {ctl, close};
        {send, Msg} ->
            error_logger:info_msg("Send XMPP ~p~n", [Msg]),
            XMPP ! {send, Msg},
            xmpp(in_room, Bridge, XMPP, Users);
        
        {msg, Msg} ->
            case sxml_read_string(Msg) of 
                {message, [{from, From}, {to, _To}, {type,"groupchat"}, {id,_Id}], 
                 [{body,[],[Text]} | _Fluff]} ->
                    User = lists:last(string:tokens(From,"/")),
                    Bridge ! {xmpp_msg,User,Text},
                    xmpp(in_room, Bridge, XMPP, Users);
                _Else  ->
                    %%error_logger:info_msg("Ignoring: ~p - ~n", [_Else]),
                    xmpp(in_room, Bridge, XMPP, Users)
            end
    end.

%% @doc IRC_FSM
%% TODO turn in to proper fs
bridge(XMPP, IRC) ->
    receive 
        {irc, NIRC} ->
            ?MODULE:bridge(XMPP, NIRC);

        {xmpp, NXMPP} ->
            ?MODULE:bridge(NXMPP, IRC);

        {xmpp_msg, Usr, Msg} ->
            IRC ! {send, ?STR("PRIVMSG ##econnrefused :~s: ~s\n",[Usr,Msg])},
            ?MODULE:bridge(XMPP, IRC);

        {irc_msg, Usr, Msg} ->
            XMPP ! {send, dh_jabber:msg_muc("web@conference.econnrefused.com",
                                           Usr++": "++Msg)},
            ?MODULE:bridge(XMPP, IRC);

        shutdown ->
            XMPP ! {ctl, close},
            IRC  ! {ctl, close}
    end.

sxml_read_string(Str) ->
  Options = [{space,normalize},{encoding,"utf-8"}],
  {XML,_Rest} = xmerl_scan:string(Str,Options),
  xmerl_lib:simplify_element(XML).
