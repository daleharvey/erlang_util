%% @doc Format dates in erlang
%% 
%% This module formats erlang dates in the form
%% {{Year, Month, Day}, {Hour, Minute, Second}}
%% to printable strings, using (almost) equivalent
%% formatting rules as http://uk.php.net/date
%%
%% erlang has no concept of timezone so the following
%% formats are not implemented: B e I O P T Z
%% formats c and r will also differ slightly
%% 
%% See tests at bottom for examples

-module(dh_date).
-author("Dale Harvey <dale@arandomurl.com>").

-define(NOTEST, 1).
-include_lib("eunit/include/eunit.hrl").

-export([format/1, format/2]).

-import(calendar,[last_day_of_the_month/2]).
-import(calendar,[day_of_the_week/1]).
-import(calendar,[datetime_to_gregorian_seconds/1]).
-import(calendar,[date_to_gregorian_days/1]).
-import(calendar,[gregorian_days_to_date/1]).
-import(calendar,[is_leap_year/1]).

-type year()     :: non_neg_integer().
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-type date()     :: {year(),month(),day()}.
-type time()     :: {hour(),minute(),second()}.
-type datetime() :: {date(),time()}.
-type now()      :: {integer(),integer(),integer()}.
%%
%% EXPORTS
%%

-spec format(string()) -> string().
%% @doc format current local time as Format
format(Format) ->
    format(Format,calendar:universal_time(),[]).

-spec format(string(),datetime() | now()) -> string().
%% @doc format Date as Format
format(Format, {_,_,_}=Now) ->
    format(Format, calendar:now_to_datetime(Now), []);
format(Format, Date) ->
    format(Format, Date, []).

%%
%% LOCAL FUNCTIONS
%%

-spec format(string(),datetime(),list()) -> string().
%% Finished, return
format([], _Date, Acc) ->
    lists:flatten(lists:reverse(Acc));

%% Escape backslashes
format([$\\,H|T], Dt, Acc) ->
    format(T,Dt,[H|Acc]);

%% Year Formats
format([$Y|T], {{Y,_,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(Y)|Acc]);
format([$y|T], {{Y,_,_},_}=Dt, Acc) ->
    [_, _, Y3, Y4] = itol(Y),
    format(T, Dt, [[Y3,Y4]|Acc]);
format([$L|T], {{Y,_,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(is_leap(Y))|Acc]);
format([$o|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(iso_year(Date))|Acc]);

%% Month Formats
format([$n|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(M)|Acc]);
format([$m|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$M|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [smonth(M)|Acc]);
format([$F|T], {{_,M,_},_}=Dt, Acc) ->
    format(T, Dt, [month(M)|Acc]);
format([$t|T], {{Y,M,_},_}=Dt, Acc) ->
    format(T, Dt, [itol(last_day_of_the_month(Y,M))|Acc]);

%% Week Formats
format([$W|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [pad2(iso_week(Date))|Acc]);

%% Day Formats
format([$j|T], {{_,_,D},_}=Dt, Acc) ->
    format(T, Dt, [pad2(D)|Acc]);
format([$S|T], {{_,_,D},_}=Dt, Acc) ->
    format(T, Dt,[suffix(D)| Acc]);
format([$d|T], {{_,_,D},_}=Dt, Acc) ->
    format(T, Dt, [itol(D)|Acc]);
format([$D|T], {Date,_}=Dt, Acc) ->
    io:format("~p",[Date]),
    format(T, Dt, [sdayd(Date)|Acc]);
format([$l|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [day(day_of_the_week(Date))|Acc]);
format([$N|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(day_of_the_week(Date))|Acc]);
format([$w|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(to_w(day_of_the_week(Date)))|Acc]);
format([$z|T], {Date,_}=Dt, Acc) ->
    format(T, Dt, [itol(days_in_year(Date))|Acc]);

%% Time Formats
format([$a|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, ["pm"|Acc]);
format([$a|T], Dt, Acc) ->
    format(T, Dt, ["am"|Acc]);
format([$A|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, ["PM"|Acc]);
format([$A|T], Dt, Acc) ->
    format(T, Dt, ["AM"|Acc]);
format([$g|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [itol(H-12)|Acc]);
format([$g|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$G|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$h|T], {_,{H,_,_}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [pad2(H-12)|Acc]);
format([$h|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$H|T], {_,{H,_,_}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$i|T], {_,{_,M,_}}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$s|T], {_,{_,_,S}}=Dt, Acc) ->
    format(T, Dt, [pad2(S)|Acc]);

%% Whole Dates
format([$c|T], {{Y,M,D},{H,Min,S}}=Dt, Acc) ->
    Format = "~4.10.0B-~2.10.0B-~2.10.0B"
        ++" ~2.10.0B:~2.10.0B:~2.10.0B",
    Date = io_lib:format(Format, [Y, M, D, H, Min, S]),
    format(T, Dt, [Date|Acc]);
format([$r|T], {{Y,M,D},{H,Min,S}}=Dt, Acc) ->
    Format = "~s, ~p ~s ~p ~2.10.0B:~2.10.0B:~2.10.0B",
    Args   = [sdayd({Y,M,D}), D, smonth(M), Y, H, Min, S],
    format(T, Dt, [io_lib:format(Format, Args)|Acc]);
format([$U|T], Dt, Acc) ->
    Epoch = {{1970,1,1},{0,0,0}},
    Time  = datetime_to_gregorian_seconds(Dt) -
        datetime_to_gregorian_seconds(Epoch),
    format(T, Dt, [itol(Time)|Acc]);

%% Unrecognised, print as is
format([H|T], Date, Acc) ->
    format(T, Date, [H|Acc]).


%% @doc days in year
-spec days_in_year(date()) -> integer().
days_in_year({Y,_,_}=Date) -> 
    date_to_gregorian_days(Date) - 
        date_to_gregorian_days({Y,1,1}).

%% @doc is a leap year
-spec is_leap(year()) -> 1|0.
is_leap(Y) ->    
    case is_leap_year(Y) of
        true  -> 1;
        false -> 0
    end.

%% @doc Made up numeric day of the week 
%%      (0 Sunday -> 6 Saturday)
-spec to_w(daynum()) -> integer().
to_w(7) -> 0;
to_w(X) -> X.

-spec suffix(day()) -> string().
%% @doc English ordinal suffix for the day of the 
%%      month, 2 characters
suffix(1) -> "st";
suffix(2) -> "nd";
suffix(3) -> "rd";
suffix(_) -> "th".

-spec sdayd(date()) -> string().
%% @doc A textual representation of a day, three letters
sdayd({Y,M,D}) -> 
    sday(day_of_the_week({Y,M,D})).

-spec sday(daynum()) -> string().
%% @doc A textual representation of a day, three letters
sday(1) -> "Mon";
sday(2) -> "Tue";
sday(3) -> "Wed";
sday(4) -> "Thu";
sday(5) -> "Fri";
sday(6) -> "Sat";
sday(7) -> "Sun".

-spec day(daynum()) -> string().
%% @doc A full textual representation of a day
day(1) -> "Monday";
day(2) -> "Tuesday";
day(3) -> "Wednesday";
day(4) -> "Thursday";
day(5) -> "Friday";
day(6) -> "Saturday";
day(7) -> "Sunday".

-spec smonth(month()) -> string().
%% @doc A short textual representation of a 
%%      month, three letters
smonth(1)  -> "Jan";
smonth(2)  -> "Feb";
smonth(3)  -> "Mar";
smonth(4)  -> "Apr";
smonth(5)  -> "May";
smonth(6)  -> "Jun";
smonth(7)  -> "Jul";
smonth(8)  -> "Aug";
smonth(9)  -> "Sep";
smonth(10) -> "Oct";
smonth(11) -> "Nov";
smonth(12) -> "Dec".

-spec month(month()) -> string().
%% @doc A full textual representation of a month
month(1)  -> "January";
month(2)  -> "February";
month(3)  -> "March";
month(4)  -> "April";
month(5)  -> "May";
month(6)  -> "June";
month(7)  -> "July";
month(8)  -> "August";
month(9)  -> "September";
month(10) -> "October";
month(11) -> "November";
month(12) -> "December".

-spec iso_week(date()) -> integer().
%% @doc The week of the years as defined in ISO 8601
%%      http://en.wikipedia.org/wiki/ISO_week_date
iso_week(Date) ->
    Week = iso_week_one(iso_year(Date)),
    Days = date_to_gregorian_days(Date) - 
        date_to_gregorian_days(Week),
    trunc((Days / 7) + 1).

-spec iso_year(date()) -> integer().
%% @doc The year number as defined in ISO 8601
%%      http://en.wikipedia.org/wiki/ISO_week_date
iso_year({Y, _M, _D}=Dt) ->
    case Dt >= {Y, 12, 29} of 
        true ->
            case Dt < iso_week_one(Y+1) of
                true  -> Y;
                false -> Y+1
            end;
        false ->
            case Dt < iso_week_one(Y) of
                true  -> Y-1;
                false -> Y
            end
    end.

-spec iso_week_one(year()) -> date().
%% @doc The date of the the first day of the first week 
%%      in the ISO calendar
iso_week_one(Y) ->
    Day1 = calendar:day_of_the_week({Y,1,4}),
    Days = date_to_gregorian_days({Y,1,4}) + (1-Day1),
    gregorian_days_to_date(Days).

-spec itol(integer()) -> list().
%% @doc short hand
itol(X) -> 
    integer_to_list(X).

-spec pad2(integer()) -> list().
%% @doc int padded with 0 to make sure its 2 chars
pad2(X) -> 
    io_lib:format("~2.10.0B",[X]).


%%
%% TEST FUNCTIONS
%%
%% c(dh_date,[{d,'TEST'}]).

-define(DATE, {{2001,3,10},{17,16,17}}).
-define(ISO,  "o \\WW").

basic_test() -> [
  ?assertEqual(format("F j, Y, g:i a",?DATE),
               "March 10, 2001, 5:16 pm"),
  ?assertEqual(format("m.d.y",?DATE),
               "03.10.01"),
  ?assertEqual(format("j, n, Y",?DATE),
               "10, 3, 2001"),
  ?assertEqual(format("Ymd",?DATE),
               "20010310"),
  ?assertEqual(format("h-i-s, j-m-y, it is w Day",?DATE),
               "05-16-17, 10-03-01, 1631 1617 6 Satpm01"),
  ?assertEqual(format("\\i\\t \\i\\s \\t\\h\\e\\ jS \\d\\a\\y.",?DATE),
               "it is the 10th day."),
  ?assertEqual(format("D M j G:i:s Y",?DATE),
               "Sat Mar 10 17:16:17 2001"),
  ?assertEqual(format("H:m:s \\m \\i\\s \\m\\o\\n\\t\\h",?DATE),
               "17:03:17 m is month"),
  ?assertEqual(format("H:i:s",?DATE),
               "17:16:17"),
  ?assertEqual(format("z",?DATE),
               "68")
].

iso_test() -> [
  ?assertEqual("2004 W53",format(?ISO,{{2005,1,1},  {1,1,1}})),
  ?assertEqual("2004 W53",format(?ISO,{{2005,1,2},  {1,1,1}})),
  ?assertEqual("2005 W52",format(?ISO,{{2005,12,31},{1,1,1}})), 
  ?assertEqual("2007 W01",format(?ISO,{{2007,1,1},  {1,1,1}})),
  ?assertEqual("2007 W52",format(?ISO,{{2007,12,30},{1,1,1}})),
  ?assertEqual("2008 W01",format(?ISO,{{2007,12,31},{1,1,1}})),
  ?assertEqual("2008 W01",format(?ISO,{{2008,1,1},  {1,1,1}})),
  ?assertEqual("2009 W01",format(?ISO,{{2008,12,29},{1,1,1}})),
  ?assertEqual("2009 W01",format(?ISO,{{2008,12,31},{1,1,1}})),
  ?assertEqual("2009 W01",format(?ISO,{{2009,1,1},  {1,1,1}})),
  ?assertEqual("2009 W53",format(?ISO,{{2009,12,31},{1,1,1}})),
  ?assertEqual("2009 W53",format(?ISO,{{2010,1,3},  {1,1,1}}))
].
