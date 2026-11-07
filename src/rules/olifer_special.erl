-module(olifer_special).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

-define(NOT_WHITESPACE, "[^" ?WHITESPACE "]").

%% API
-export([email/3]).
-export([url/3]).
-export([iso_date/3]).
-export([iso_datetime/3]).
-export([iso_time/3]).
-export([equal_to_field/3]).
%% For the schema check
-export([iso_date_args/1, date_bound/1, datetime_bound/2, parse_time/1]).

-define(DAY_MS, 86400000).
%% 10000-01-01T00:00:00Z: the output of iso_datetime has years 0000-9999 only
-define(MAX_INSTANT, 3652425 * ?DAY_MS).

email(<<>> = Value, _Args, _) ->
    {ok, Value};
email(Value, [], _) when is_binary(Value) ->
    %% The regular expression of the reference implementation: one "@" only, no "_" in the domain,
    %% no whitespace (and no trailing newline: dollar_endonly)
    Pattern = "^(?:[^<>()\\[\\]\\\\.,;:" ?WHITESPACE "@\"]+(?:\\.[^<>()\\[\\]\\\\.,;:" ?WHITESPACE "@\"]+)*"
              "|\"[^@\\n\\r\\x{2028}\\x{2029}]+\")"
              "@(?:\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\]|(?:[a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,})$",
    case matches(Value, Pattern, []) of
        false -> {error, ?WRONG_EMAIL};
        true -> {ok, Value}
    end;
email(_, _, _) ->
    {error, ?FORMAT_ERROR}.

url(<<>> = Value, _Args, _) ->
    {ok, Value};
url(Value, [], _) when is_binary(Value) ->
    %% The regular expression of the reference implementation: an IPv4 address, localhost or
    %% a host name with a top level domain, an optional port of 2-5 digits, no whitespace
    Pattern = "^(?:(?:http|https)://)(?:" ?NOT_WHITESPACE "+(?::" ?NOT_WHITESPACE "*)?@)?"
              "(?:(?:(?:[1-9]\\d?|1\\d\\d|2[0-1]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}"
              "(?:\\.(?:[0-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))"
              "|(?:(?:[a-z\\x{A1}-\\x{10FFFF}0-9]-*)*[a-z\\x{A1}-\\x{10FFFF}0-9]+)"
              "(?:\\.(?:[a-z\\x{A1}-\\x{10FFFF}0-9]-*)*[a-z\\x{A1}-\\x{10FFFF}0-9]+)*"
              "(?:\\.(?:[a-z\\x{A1}-\\x{10FFFF}]{2,})))\\.?|localhost)"
              "(?::\\d{2,5})?(?:[/?#]" ?NOT_WHITESPACE "*)?$",
    IsUrl = case unicode:characters_to_list(Value) of
        Chars when is_list(Chars), length(Chars) < 2083 -> matches(Value, Pattern, [caseless]);
        _ -> false
    end,
    case IsUrl of
        false -> {error, ?WRONG_URL};
        true -> {ok, Value}
    end;
url(_, _, _) ->
    {error, ?FORMAT_ERROR}.

%% "iso_date" and {"iso_date": []} as in LIVR 2.0, or {"iso_date": {"min": ..., "max": ...}}
%% with dates or "today", "yesterday", "tomorrow" (LIVR 2.5)
iso_date(Value, _Args, _) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
iso_date(Value, Args, _) when is_binary(Value) ->
    case parse_date(Value) of
        error -> {error, ?WRONG_DATE};
        Date -> check_date_bounds(Date, options(iso_date_args(Args)), Value)
    end;
iso_date(Value, _Args, _) ->
    not_string(Value, ?WRONG_DATE).

%% The arguments of iso_date as a list. LIVR 2.0 ignored them, so a single null ({"iso_date": null}
%% or {"iso_date": [null]}) stays the plain iso_date, like []
iso_date_args(Args) ->
    case olifer:args(Args) of
        [null] -> [];
        List -> List
    end.

%% A date and a time with an offset, the output is the instant in UTC: YYYY-MM-DDTHH:MM:SS.sssZ
iso_datetime(Value, _Args, _) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
iso_datetime(Value, Args, _) when is_binary(Value) ->
    case parse_datetime(Value) of
        {ok, Instant} when Instant >= 0, Instant < ?MAX_INSTANT ->
            check_datetime_bounds(Instant, options(olifer:args(Args)), Value);
        _ ->
            {error, ?WRONG_DATETIME}
    end;
iso_datetime(Value, _Args, _) ->
    not_string(Value, ?WRONG_DATETIME).

%% HH:MM, HH:MM:SS or HH:MM:SS.fraction, the value does not change
iso_time(Value, _Args, _) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
iso_time(Value, Args, _) when is_binary(Value) ->
    case parse_time(Value) of
        error -> {error, ?WRONG_TIME};
        Time -> check_time_bounds(Time, options(olifer:args(Args)), Value)
    end;
iso_time(Value, _Args, _) ->
    not_string(Value, ?WRONG_TIME).

%% A date bound: {date, Date} or {days, Days} from the current date
date_bound(<<"yesterday">>) -> {days, -1};
date_bound(<<"today">>) -> {days, 0};
date_bound(<<"tomorrow">>) -> {days, 1};
date_bound(Bound) when is_binary(Bound) ->
    case parse_date(Bound) of
        error -> error;
        Date -> {date, Date}
    end;
date_bound(_Bound) ->
    error.

%% A datetime bound: {instant, Milliseconds} or {days, Days} from the current date.
%% A date is the start of the day as "min" and the end of it as "max"
datetime_bound(Bound, Option) ->
    case date_bound(Bound) of
        {date, Date} -> {instant, day_start(calendar:date_to_gregorian_days(Date), Option)};
        {days, Days} -> {days, Days};
        error when is_binary(Bound) ->
            case parse_datetime(Bound) of
                {ok, Instant} -> {instant, Instant};
                error -> error
            end;
        error -> error
    end.

%% A time of day in nanoseconds
parse_time(Value) when is_binary(Value) ->
    case re:run(Value, "^([0-9]{2}):([0-9]{2})(?::([0-9]{2})(?:\\.([0-9]{1,9}))?)?$",
                [dollar_endonly, {capture, all_but_first, list}]) of
        {match, [Hours, Minutes | SecondsAndFraction]} ->
            {Seconds, Fraction} = case SecondsAndFraction of
                [] -> {"00", ""};
                [S] -> {S, ""};
                [S, F] -> {S, F}
            end,
            case {list_to_integer(Hours), list_to_integer(Minutes), list_to_integer(Seconds)} of
                {H, M, S1} when H =< 23, M =< 59, S1 =< 59 ->
                    ((H * 60 + M) * 60 + S1) * 1000000000 + fraction(Fraction, 9);
                _ ->
                    error
            end;
        nomatch ->
            error
    end;
parse_time(_Value) ->
    error.

equal_to_field(<<>> = Value, _Args, _) ->
    {ok, Value};
equal_to_field(Value, [FieldName], AllData) when is_binary(Value); is_integer(Value); is_float(Value) ->
    equal_to_field(Value, FieldName, AllData);
equal_to_field(Value, FieldName, AllData) when is_binary(Value); is_integer(Value); is_float(Value) ->
    FieldValue = olifer:get_value(FieldName, AllData),
    case olifer:to_binary(Value) =:= olifer:to_binary(FieldValue) of
        true -> {ok, Value};
        false -> {error, ?FIELDS_NOT_EQUAL}
    end;
equal_to_field(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.


%% INTERNAL
%% A binary that is not valid UTF-8 does not match
matches(Value, Pattern, Opts) ->
    try re:run(Value, Pattern, [unicode, dollar_endonly, {capture, none} | Opts]) of
        match -> true;
        nomatch -> false
    catch
        error:badarg -> false
    end.

%% A number or a boolean is never a date or a time
not_string(Value, Error) ->
    case olifer:is_primitive(Value) of
        true -> {error, Error};
        false -> {error, ?FORMAT_ERROR}
    end.

%% The options object of iso_date, iso_datetime and iso_time, [{}] without options
options([]) -> [{}];
options([Options | _]) -> Options.

%% YYYY-MM-DD, a valid calendar date
parse_date(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary>>) ->
    case {digits(Year), digits(Month), digits(Day)} of
        {Y, M, D} when is_integer(Y), is_integer(M), is_integer(D) ->
            case calendar:valid_date(Y, M, D) of
                true -> {Y, M, D};
                false -> error
            end;
        _ ->
            error
    end;
parse_date(_Value) ->
    error.

%% ASCII digits as an integer
digits(Bin) ->
    case [Char || <<Char>> <= Bin, Char < $0 orelse Char > $9] of
        [] -> binary_to_integer(Bin);
        _ -> error
    end.

%% The first digits of a fraction as an integer: fraction("5", 3) -> 500, fraction("1239", 3) -> 123
fraction(Digits, Length) ->
    list_to_integer(string:slice(Digits ++ lists:duplicate(Length, $0), 0, Length)).

%% YYYY-MM-DD "T" HH ":" MM [ ":" SS [ "." 1*9DIGIT ] ] ( "Z" / ( "+" / "-" ) HH ":" MM ),
%% the instant in milliseconds since 0000-01-01T00:00:00Z, cut to milliseconds
parse_datetime(Value) ->
    Pattern = "^([0-9]{4}-[0-9]{2}-[0-9]{2})T([0-9]{2}:[0-9]{2}(?::[0-9]{2}(?:\\.[0-9]{1,9})?)?)"
              "(?:Z|([+-])([0-9]{2}):([0-9]{2}))$",
    case re:run(Value, Pattern, [dollar_endonly, {capture, all_but_first, binary}]) of
        {match, [Date, Time | Offset]} ->
            case {parse_date(Date), parse_time(Time), offset_minutes(Offset)} of
                {{_, _, _} = YMD, Nanoseconds, Minutes} when is_integer(Nanoseconds), is_integer(Minutes) ->
                    {ok, day_start(calendar:date_to_gregorian_days(YMD), min)
                         + Nanoseconds div 1000000 - Minutes * 60000};
                _ ->
                    error
            end;
        nomatch ->
            error
    end.

offset_minutes([]) ->
    0;
offset_minutes([Sign, Hours, Minutes]) ->
    case {binary_to_integer(Hours), binary_to_integer(Minutes)} of
        {H, M} when H =< 23, M =< 59, Sign =:= <<"-">> -> -(H * 60 + M);
        {H, M} when H =< 23, M =< 59 -> H * 60 + M;
        _ -> error
    end.

day_start(Days, min) -> Days * ?DAY_MS;
day_start(Days, max) -> (Days + 1) * ?DAY_MS - 1.

format_instant(Instant) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(Instant div 1000),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                                   [Y, Mo, D, H, Mi, S, Instant rem 1000])).

%% The current date in UTC, moved by Days
current_date(Days) ->
    {Today, _} = calendar:system_time_to_universal_time(olifer_clock:system_time(), second),
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Today) + Days).

check_date_bounds(Date, Options, Value) ->
    Min = bound_date(olifer:get_value(<<"min">>, Options)),
    Max = bound_date(olifer:get_value(<<"max">>, Options)),
    if
        Min =/= undefined, Date < Min -> {error, ?DATE_TOO_EARLY};
        Max =/= undefined, Date > Max -> {error, ?DATE_TOO_LATE};
        true -> {ok, Value}
    end.

bound_date(undefined) ->
    undefined;
bound_date(Bound) ->
    case date_bound(Bound) of
        {date, Date} -> Date;
        {days, Days} -> current_date(Days)
    end.

check_datetime_bounds(Instant, Options, Value) ->
    Min = bound_instant(olifer:get_value(<<"min">>, Options), min),
    Max = bound_instant(olifer:get_value(<<"max">>, Options), max),
    if
        Min =/= undefined, Instant < Min ->
            {error, ?DATETIME_TOO_EARLY};
        Max =/= undefined, Instant > Max ->
            {error, ?DATETIME_TOO_LATE};
        true ->
            case format_instant(Instant) of
                Value -> {ok, Value};
                Output -> {filter, Output}
            end
    end.

bound_instant(undefined, _Option) ->
    undefined;
bound_instant(Bound, Option) ->
    case datetime_bound(Bound, Option) of
        {instant, Instant} -> Instant;
        {days, Days} -> day_start(calendar:date_to_gregorian_days(current_date(Days)), Option)
    end.

%% With min later than max the range goes over midnight, and a time between them gets
%% the error of the nearer bound
check_time_bounds(Time, Options, Value) ->
    Min = bound_time(olifer:get_value(<<"min">>, Options)),
    Max = bound_time(olifer:get_value(<<"max">>, Options)),
    if
        Min =/= undefined, Max =/= undefined, Min > Max ->
            if
                Time >= Min; Time =< Max -> {ok, Value};
                Time - Max < Min - Time -> {error, ?TIME_TOO_LATE};
                true -> {error, ?TIME_TOO_EARLY}
            end;
        Min =/= undefined, Time < Min -> {error, ?TIME_TOO_EARLY};
        Max =/= undefined, Time > Max -> {error, ?TIME_TOO_LATE};
        true -> {ok, Value}
    end.

bound_time(undefined) -> undefined;
bound_time(Bound) -> parse_time(Bound).

