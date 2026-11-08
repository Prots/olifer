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
-export([max_decimals/3]).
-export([decimal_number/1]).

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
    {ok, Value};
decimal(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

positive_decimal(<<>> = Value, [], _) ->
    {ok, Value};
positive_decimal(Value, [], _) when is_number(Value), Value > 0 ->
    {ok, Value};
positive_decimal(Value, [], _) when is_number(Value), Value =< 0 ->
    {error, ?NOT_POSITIVE_DECIMAL};
positive_decimal(Value, [], _) when is_binary(Value) ->
    case binary_to_flt(Value) of
        error -> {error, ?NOT_POSITIVE_DECIMAL};
        {ok, FltValue} when FltValue > 0 -> {ok, FltValue};
        _ -> {error, ?NOT_POSITIVE_DECIMAL}
    end;
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

%% Digits after the decimal point without the trailing zeros: "1.230" and 1.230 have 2.
%% The output is a number
max_decimals(Value, _Args, _) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
max_decimals(Value, Args, _) when is_number(Value) ->
    check_decimals(Value, Value, olifer:to_binary(Value), Args);
max_decimals(Value, Args, _) when is_binary(Value) ->
    case decimal_number(Value) of
        error -> {error, ?NOT_NUMBER};
        Number -> check_decimals(Value, Number, Value, Args)
    end;
max_decimals(Value, _Args, _) when is_boolean(Value) ->
    {error, ?NOT_NUMBER};
max_decimals(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% A string in the grammar of the decimal rule, -?[0-9]+(\.[0-9]+)?, as a number that fits
%% a double (an integer without a fraction), error for other strings
decimal_number(Value) ->
    case re:run(Value, "^-?[0-9]+(\\.[0-9]+)?$", [dollar_endonly, {capture, none}]) of
        nomatch ->
            error;
        match ->
            try
                Number = case binary:match(Value, <<".">>) of
                    nomatch -> binary_to_integer(Value);
                    _ -> binary_to_float(Value)
                end,
                _ = float(Number),
                Number
            catch
                error:badarg -> error
            end
    end.

%% INTERNAL
check_decimals(Value, Number, String, Args) ->
    [MaxDecimals | _] = olifer:args(Args),
    case decimals(String) > olifer_schema:non_negative_integer(MaxDecimals) of
        true -> {error, ?TOO_MANY_DECIMALS};
        false when Number =:= Value -> {ok, Value};
        false -> {filter, Number}
    end.

%% The digits after the point of a decimal string, or of the string form of a number:
%% "1.25" has 2, "1.5e-7" has 8
decimals(String) ->
    {Mantissa, Exponent} = case binary:split(String, <<"e">>) of
        [M, E] -> {M, binary_to_integer(E)};
        [M] -> {M, 0}
    end,
    Fraction = case binary:split(Mantissa, <<".">>) of
        [_, F] -> string:trim(F, trailing, "0");
        [_] -> <<>>
    end,
    max(0, byte_size(Fraction) - Exponent).

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
