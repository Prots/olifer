-module(olifer_special).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

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

%%TODO not validate URLs with special symbols, like '_'
url(<<>> = Value, _Args, _) ->
    {ok, Value};
url(Value, [], _) when is_binary(Value) ->
    case http_uri:parse(binary_to_list(Value)) of
        {ok, ParsedUrl} -> check_url_details(ParsedUrl, Value);
        {error, _} -> {error, ?WRONG_URL}
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
    FieldValue = proplists:get_value(FieldName, AllData),
    case Value =:= FieldValue of
        true -> {ok, Value};
        false -> {error, ?FIELDS_NOT_EQUAL}
    end;
equal_to_field(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.


%% INTERNAL
check_url_details({http, _UserInfo, _Host, _Port, _Path, _Query}, Value) ->
    {ok, Value};
check_url_details({https, _UserInfo, _Host, _Port, _Path, _Query}, Value) ->
    {ok, Value};
check_url_details(_, _) ->
    {error, ?WRONG_URL}.

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

