-module(olifer_special).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

-define(NOT_WHITESPACE, "[^" ?WHITESPACE "]").

%% API
-export([email/3]).
-export([url/3]).
-export([iso_date/3]).
-export([equal_to_field/3]).

email(<<>> = Value, _Args, _) ->
    {ok, Value};
email(Value, [], _) when is_binary(Value) ->
    Pattern = "^((?:(?:[^\"@\\.\s]+\\.?)|(?:\\.\"[^\"\s]+\"\\.))*(?:(?:\\.?\"[^\"\s]+\")|(?:[a-zA-Z0-9\\-_]+)))@[a-z0-9\\.\\-\\[\\]]+$",
    case re:run(Value, Pattern, [caseless, anchored]) of
        nomatch -> {error, ?WRONG_EMAIL};
        _ -> {ok, Value}
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

iso_date(<<>> = Value, _Args, _) ->
    {ok, Value};
iso_date(Value, [], _) when is_binary(Value) ->
    case binary:split(Value, <<"-">>, [global]) of
        [_, _, _] = DateBin -> check_date(DateBin, Value);
        _ -> {error, ?WRONG_DATE}
    end;
iso_date(_, _, _) ->
    {error, ?FORMAT_ERROR}.

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

check_date(DateBin, Value) ->
    case [binary_to_int(Bin)|| Bin <- DateBin] of
        [{ok, Year}, {ok, Month}, {ok, Day}] ->
            case calendar:valid_date({Year, Month, Day}) of
                true -> {ok, Value};
                _ -> {error, ?WRONG_DATE}
            end;
        _ ->
            {error, ?WRONG_DATE}
    end.

binary_to_int(Value) ->
    try
        IntValue = binary_to_integer(Value),
        {ok, IntValue}
    catch
        _:_ -> error
    end.

