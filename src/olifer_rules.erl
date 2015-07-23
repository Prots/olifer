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
%%STRING RULES
-export([one_of/2]).
-export([max_length/2]).
-export([min_length/2]).
-export([length_between/2]).
-export([length_equal/2]).
-export([like/2]).

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
integer(Value, []) when is_integer(Value) ->
    {ok, Value};
integer(<<>> = Value, []) ->
    {ok, Value};
integer(Value, []) when is_binary(Value) ->
    binary_to_int(Value, ?NOT_INTEGER);
integer(Value, []) when is_list(Value) ->
    list_to_int(Value, ?NOT_INTEGER);
integer(_Value, _Args) ->
    {error, ?NOT_INTEGER}.

positive_integer(Value, []) ->
    case integer(Value, []) of
        ?NOT_INTEGER -> {error, ?NOT_POSITIVE_INTEGER};
        Value when Value >= 0 -> {ok, Value};
        _ -> {error, ?NOT_POSITIVE_INTEGER}
    end.

decimal(Value, []) when is_float(Value) ->
    true;
decimal(_Value, _Args) ->
    false.

positive_decimal(Value, []) when is_float(Value), Value > 0 ->
    true;
positive_decimal(_Value, _Args) ->
    false.

max_number(Value, [MaxNumber])
    when is_number(Value), Value =< MaxNumber ->
    true;
max_number(_Value, _Args) ->
    false.

min_number(Value, [MinNumber])
    when is_number(Value), Value >= MinNumber ->
    true;
min_number(_Value, _Args) ->
    false.

number_between(Value, [MinNumber, MaxNumber])
    when is_number(Value), Value >= MinNumber, Value =< MaxNumber ->
    true;
number_between(_Value, _Args) ->
    false.

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

like(Value, [_Pattern]) when is_list(Value) ->
    true;
like(_Value, _Args) ->
    {error, ?FORMAT_ERROR}.

%% INTERNAL
binary_to_int(Value, Error) ->
    try
        _ = binary_to_integer(Value),
        {ok, Value}
    catch
        _:_ -> {error, Error}
    end.

list_to_int(Value, Error) ->
    try
        _ = list_to_integer(Value),
        {ok, Value}
    catch
        _:_ -> {error, Error}
    end.