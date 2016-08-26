-module(olifer_numeric).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([integer/3]).
-export([positive_integer/3]).
-export([decimal/3]).
-export([positive_decimal/3]).
-export([max_number/3]).
-export([min_number/3]).
-export([number_between/3]).

%% API
integer(<<>> = Value, [], _) ->
    {ok, Value};
integer(Value, [], _) when is_integer(Value) ->
    {ok, Value};
integer(Value, [], _) when is_binary(Value) ->
    case binary_to_int(Value) of
        error -> {error, ?NOT_INTEGER};
        {ok, IntValue} -> {ok, IntValue}
    end;
integer(Value, [], _) when is_float(Value) ->
    {error, ?NOT_INTEGER};
integer(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

positive_integer(<<>> = Value, [], _) ->
    {ok, Value};
positive_integer(Value, [], _) when is_integer(Value), Value > 0 ->
    {ok, Value};
positive_integer(Value, [], _) when is_integer(Value), Value =< 0 ->
    {error, ?NOT_POSITIVE_INTEGER};
positive_integer(Value, [], _) when is_binary(Value) ->
    case binary_to_int(Value) of
        error -> {error, ?NOT_POSITIVE_INTEGER};
        {ok, IntValue} when IntValue > 0 -> {ok, IntValue};
        _ -> {error, ?NOT_POSITIVE_INTEGER}
    end;
positive_integer(Value, [], _) when is_float(Value) ->
    {error, ?NOT_POSITIVE_INTEGER};
positive_integer(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

decimal(<<>> = Value, [], _) ->
    {ok, Value};
decimal(Value, [], _) when is_float(Value) ->
    {ok, Value};
decimal(Value, [], _) when is_binary(Value) ->
    case binary_to_flt(Value) of
        error -> {error, ?NOT_DECIMAL};
        {ok, FltValue} -> {ok, FltValue}
    end;
decimal(Value, [], _) when is_integer(Value) ->
    {error, ?NOT_DECIMAL};
decimal(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

positive_decimal(<<>> = Value, [], _) ->
    {ok, Value};
positive_decimal(Value, [], _) when is_float(Value), Value > 0 ->
    {ok, Value};
positive_decimal(Value, [], _) when is_float(Value), Value =< 0 ->
    {error, ?NOT_POSITIVE_DECIMAL};
positive_decimal(Value, [], _) when is_binary(Value) ->
    case binary_to_flt(Value) of
        error -> {error, ?NOT_POSITIVE_DECIMAL};
        {ok, FltValue} when FltValue > 0 -> {ok, FltValue};
        _ -> {error, ?NOT_POSITIVE_DECIMAL}
    end;
positive_decimal(Value, [], _) when is_integer(Value) ->
    {error, ?NOT_POSITIVE_DECIMAL};
positive_decimal(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

max_number(<<>> = Value, _Args, _) ->
    {ok, Value};
max_number(Value, [MaxNumber], AllData) ->
    max_number(Value, MaxNumber, AllData);
max_number(Value, MaxNumber, _) when is_number(Value), Value =< MaxNumber ->
    {ok, Value};
max_number(Value, _Args, _) when is_number(Value) ->
    {error, ?TOO_HIGH};
max_number(Value, MaxNumber, _) when is_binary(Value) ->
    IntRes = binary_to_int(Value),
    FltRes = binary_to_flt(Value),
    case {IntRes, FltRes} of
        {error, error} -> {error, ?NOT_NUMBER};
        {{ok, IntValue}, _} when IntValue =< MaxNumber -> {ok, IntValue};
        {_, {ok, FltValue}} when FltValue =< MaxNumber -> {ok, FltValue};
        _ -> {error, ?TOO_HIGH}
    end;
max_number(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

min_number(<<>> = Value, _Args, _) ->
    {ok, Value};
min_number(Value, [MinNumber], AllData) ->
    min_number(Value, MinNumber, AllData);
min_number(Value, MinNumber, _) when is_number(Value), Value >= MinNumber ->
    {ok, Value};
min_number(Value, _Args, _) when is_number(Value) ->
    {error, ?TOO_LOW};
min_number(Value, MinNumber, _) when is_binary(Value) ->
    IntRes = binary_to_int(Value),
    FltRes = binary_to_flt(Value),
    case {IntRes, FltRes} of
        {error, error} -> {error, ?NOT_NUMBER};
        {{ok, IntValue}, _} when IntValue >= MinNumber -> {ok, IntValue};
        {_, {ok, FltValue}} when FltValue >= MinNumber -> {ok, FltValue};
        _ -> {error, ?TOO_LOW}
    end;
min_number(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

number_between(<<>> = Value, _Args, _) ->
    {ok, Value};
number_between(Value, [[MinNumber, MaxNumber]], AllData) ->
    number_between(Value, [MinNumber, MaxNumber], AllData);
number_between(Value, [MinNumber, MaxNumber], _)
    when is_number(Value), Value >= MinNumber, Value =< MaxNumber ->
    {ok, Value};
number_between(Value, [MinNumber, _], _) when is_number(Value), Value < MinNumber ->
    {error, ?TOO_LOW};
number_between(Value, [_, MaxNumber], _) when is_number(Value), Value > MaxNumber ->
    {error, ?TOO_HIGH};
number_between(Value, [MinNumber, MaxNumber], _) when is_binary(Value) ->
    IntRes = binary_to_int(Value),
    FltRes = binary_to_flt(Value),
    case {IntRes, FltRes} of
        {error, error} -> {error, ?NOT_NUMBER};
        {{ok, IntValue}, _} when IntValue > MaxNumber -> {error, ?TOO_HIGH};
        {{ok, IntValue}, _} when IntValue < MinNumber -> {error, ?TOO_LOW};
        {_, {ok, FltValue}} when FltValue > MaxNumber -> {error, ?TOO_HIGH};
        {_, {ok, FltValue}} when FltValue < MinNumber -> {error, ?TOO_LOW};
        {{ok, IntValue}, _} -> {ok, IntValue};
        {_, {ok, FltValue}} -> {ok, FltValue}
    end;
number_between(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% INTERNAL
binary_to_int(Value) ->
    try
        IntValue = binary_to_integer(Value),
        {ok, IntValue}
    catch
        _:_ -> error
    end.

%% TODO REFACTOR!
binary_to_flt(Value) ->
    case binary:match(Value, <<",">>) of
        nomatch ->
            try
                FltValue = binary_to_float(Value),
                {ok, FltValue}
            catch
                _:_ ->
                    try
                        FltValue1 = binary_to_integer(Value),
                        {ok, FltValue1}
                    catch
                        _:_ -> error
                    end
            end;
        _ -> error
    end.
