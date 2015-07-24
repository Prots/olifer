-module(olifer_rules).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% BASIC RULES
-export([required/2]).
-export([not_empty/2]).
-export([not_empty_list/2]).
%% NUMBER RULES
-export([integer/2]).
-export([positive_integer/2]).
-export([decimal/2]).
-export([positive_decimal/2]).
-export([max_number/2]).
-export([min_number/2]).
-export([number_between/2]).
%% STRING RULES
-export([one_of/2]).
-export([max_length/2]).
-export([min_length/2]).
-export([length_between/2]).
-export([length_equal/2]).
-export([like/2]).
%% SPECIAL RULES
-export([email/2]).
%% -export([url/2]).
%% -export([iso_date/2]).
-export([equal_to_field/2]).

%% BASIC RULES
required(<<>>, []) ->
    {error, ?REQUIRED};
required(null, []) ->
    {error, ?REQUIRED};
required(undefined, []) ->
    {error, ?REQUIRED};
required(Value, []) ->
    {ok, Value}.

not_empty(<<>>, []) ->
    {error, ?CANNOT_BE_EMPTY};
not_empty(Value, []) ->
    {ok, Value}.

not_empty_list(_Value, []) ->
    true.

%% NUMBER RULES
integer(<<>> = Value, []) ->
    {ok, Value};
integer(Value, []) when is_integer(Value) ->
    {ok, Value};
integer(Value, []) when is_binary(Value) ->
    case binary_to_int(Value) of
        error -> {error, ?NOT_INTEGER};
        {ok, _} -> {ok, Value}
    end;
integer(Value, []) when is_float(Value) ->
    {error, ?NOT_INTEGER};
integer(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

positive_integer(<<>> = Value, []) ->
    {ok, Value};
positive_integer(Value, []) when is_integer(Value), Value > 0 ->
    {ok, Value};
positive_integer(Value, []) when is_integer(Value), Value =< 0 ->
    {error, ?NOT_POSITIVE_INTEGER};
positive_integer(Value, []) when is_binary(Value) ->
    case binary_to_int(Value) of
        error -> {error, ?NOT_POSITIVE_INTEGER};
        {ok, IntValue} when IntValue > 0 -> {ok, Value};
        _ -> {error, ?NOT_POSITIVE_INTEGER}
    end;
positive_integer(Value, []) when is_float(Value) ->
    {error, ?NOT_POSITIVE_INTEGER};
positive_integer(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

decimal(<<>> = Value, []) ->
    {ok, Value};
decimal(Value, []) when is_float(Value) ->
    {ok, Value};
decimal(Value, []) when is_binary(Value) ->
    case binary_to_flt(Value) of
        error -> {error, ?NOT_DECIMAL};
        {ok, _} -> {ok, Value}
    end;
decimal(Value, []) when is_integer(Value) ->
    {error, ?NOT_DECIMAL};
decimal(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

positive_decimal(<<>> = Value, []) ->
    {ok, Value};
positive_decimal(Value, []) when is_float(Value), Value > 0 ->
    {ok, Value};
positive_decimal(Value, []) when is_float(Value), Value =< 0 ->
    {error, ?NOT_POSITIVE_DECIMAL};
positive_decimal(Value, []) when is_binary(Value) ->
    case binary_to_flt(Value) of
        error -> {error, ?NOT_POSITIVE_DECIMAL};
        {ok, FltValue} when FltValue > 0 -> {ok, Value};
        _ -> {error, ?NOT_POSITIVE_DECIMAL}
    end;
positive_decimal(Value, []) when is_integer(Value) ->
    {error, ?NOT_POSITIVE_DECIMAL};
positive_decimal(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

max_number(<<>> = Value, _Args) ->
    {ok, Value};
max_number(Value, [MaxNumber]) ->
    max_number(Value, MaxNumber);
max_number(Value, MaxNumber) when is_number(Value), Value =< MaxNumber ->
    {ok, Value};
max_number(Value, _Args) when is_number(Value) ->
    {error, ?TOO_HIGH};
max_number(Value, MaxNumber) when is_binary(Value) ->
    IntRes = binary_to_int(Value),
    FltRes = binary_to_flt(Value),
    case {IntRes, FltRes} of
        {error, error} -> ?FORMAT_ERROR;
        {{ok, IntValue}, _} when IntValue =< MaxNumber -> {ok, Value};
        {_, {ok, FltValue}} when FltValue =< MaxNumber -> {ok, Value};
        _ -> {error, ?TOO_HIGH}
    end;
max_number(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

min_number(<<>> = Value, _Args) ->
    {ok, Value};
min_number(Value, [MinNumber]) ->
    min_number(Value, MinNumber);
min_number(Value, MinNumber) when is_number(Value), Value >= MinNumber ->
    {ok, Value};
min_number(Value, _Args) when is_number(Value) ->
    {error, ?TOO_LOW};
min_number(Value, MinNumber) when is_binary(Value) ->
    IntRes = binary_to_int(Value),
    FltRes = binary_to_flt(Value),
    case {IntRes, FltRes} of
        {error, error} -> ?FORMAT_ERROR;
        {{ok, IntValue}, _} when IntValue >= MinNumber -> {ok, Value};
        {_, {ok, FltValue}} when FltValue >= MinNumber -> {ok, Value};
        _ -> {error, ?TOO_LOW}
    end;
min_number(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

number_between(<<>> = Value, _Args) ->
    {ok, Value};
number_between(Value, [[MinNumber, MaxNumber]]) ->
    number_between(Value, [MinNumber, MaxNumber]);
number_between(Value, [MinNumber, MaxNumber])
        when is_number(Value), Value >= MinNumber, Value =< MaxNumber ->
    {ok, Value};
number_between(Value, [MinNumber, _]) when is_number(Value), Value < MinNumber ->
    {error, ?TOO_LOW};
number_between(Value, [_, MaxNumber]) when is_number(Value), Value > MaxNumber ->
    {error, ?TOO_HIGH};
number_between(Value, [MinNumber, MaxNumber]) when is_binary(Value) ->
    IntRes = binary_to_int(Value),
    FltRes = binary_to_flt(Value),
    case {IntRes, FltRes} of
        {error, error} -> ?FORMAT_ERROR;
        {{ok, IntValue}, _} when IntValue > MaxNumber -> {error, ?TOO_HIGH};
        {{ok, IntValue}, _} when IntValue < MinNumber -> {error, ?TOO_LOW};
        {_, {ok, FltValue}} when FltValue > MaxNumber -> {error, ?TOO_HIGH};
        {_, {ok, FltValue}} when FltValue < MinNumber -> {error, ?TOO_LOW};
        _ -> {ok, Value}
    end;
number_between(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

%% STRING RULES
one_of(<<>> = Value, _Args) ->
    {ok, Value};
one_of(Value, Args) when is_binary(Value) ->
    case lists:member(Value, lists:flatten(Args)) of
        true -> {ok, Value};
        false -> {error, ?NOT_ALLOWED_VALUE}
    end;
one_of(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

max_length(<<>> = Value, _Args) ->
    {ok, Value};
max_length(Value, [MaxLength]) when is_binary(Value) ->
    max_length(Value, MaxLength);
max_length(Value, MaxLength) when is_binary(Value) ->
    case length(unicode:characters_to_list(Value)) > MaxLength of
        true -> {error, ?TOO_LONG};
        false -> {ok, Value}
    end;
max_length(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

min_length(<<>> = Value, _Args) ->
    {ok, Value};
min_length(Value, [MinLength]) when is_binary(Value) ->
    min_length(Value, MinLength);
min_length(Value, MinLength) when is_binary(Value) ->
    case length(unicode:characters_to_list(Value)) < MinLength of
        true -> {error, ?TOO_SHORT};
        false -> {ok, Value}
    end;
min_length(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

length_between(<<>> = Value, _Args) ->
    {ok, Value};
length_between(Value, [MinLength, MaxLength]) when is_binary(Value) ->
    ValueLength = length(unicode:characters_to_list(Value)),
    if
        ValueLength > MaxLength -> {error, ?TOO_LONG};
        ValueLength < MinLength -> {error, ?TOO_SHORT};
        true -> {ok, Value}
    end;
length_between(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

length_equal(<<>> = Value, _Args) ->
    {ok, Value};
length_equal(Value, [Length]) when is_binary(Value) ->
    length_equal(Value, Length);
length_equal(Value, Length) when is_binary(Value) ->
    ValueLength = length(unicode:characters_to_list(Value)),
    if
        ValueLength < Length -> {error, ?TOO_SHORT};
        ValueLength > Length -> {error, ?TOO_LONG};
        ValueLength == Length -> {ok, Value}
    end;
length_equal(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

like(<<>> = Value, _Args) ->
    {ok, Value};
like(Value, [Pattern]) when is_binary(Value) ->
    like(Value, Pattern);
like(Value, [Pattern, <<"i">>]) when is_binary(Value) ->
    like_impl(Value, Pattern, [unicode, caseless]);
like(Value, Pattern) when is_binary(Value) ->
    like_impl(Value, Pattern, [unicode]);
like(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

%% SPECIAL RULES
email(<<>> = Value, _Args) ->
    {ok, Value};
email(Value, []) when is_binary(Value) ->
    Pattern = "^((?:(?:[^\"@\\.\s]+\\.?)|(?:\\.\"[^\"\s]+\"\\.))*(?:(?:\\.?\"[^\"\s]+\")|(?:[a-zA-Z0-9\\-_]+)))@[a-z0-9\\.\\-\\[\\]]+$",
    case re:run(Value, Pattern, [caseless, anchored]) of
        nomatch -> {error, ?WRONG_EMAIL};
        _ -> {ok, Value}
    end;
email(_, _) ->
    {error, ?FORMAT_ERROR}.

equal_to_field(<<>> = Value, _Args) ->
    {ok, Value};


%% INTERNAL
binary_to_int(Value) ->
    try
        IntValue = binary_to_integer(Value),
        {ok, IntValue}
    catch
        _:_ -> error
    end.

binary_to_flt(Value) ->
    try
        FltValue = binary_to_float(Value),
        {ok, FltValue}
    catch
        _:_ -> error
    end.

like_impl(Value, Pattern, Opts) ->
    case re:compile(Pattern, Opts) of
        {ok, MP} ->
            case re:run(Value, MP) of
                nomatch -> {error, ?WRONG_FORMAT};
                _ -> {ok, Value}
            end;
        {error, _} -> {error, ?FORMAT_ERROR}
    end.