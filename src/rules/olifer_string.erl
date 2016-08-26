-module(olifer_string).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([
    string/3,
    eq/3,
    one_of/3,
    max_length/3,
    min_length/3,
    length_between/3,
    length_equal/3,
    like/3
]).

%% API
string(<<>> = Value, _Args, _) ->
    {ok, Value};
string(Value, _Args, _) when is_binary(Value) ->
    {ok, Value};
string(Value, _Args, _) when is_float(Value) ->
    {ok, float_to_binary(Value)};
string(Value, _Args, _) when is_integer(Value) ->
    {ok, integer_to_binary(Value)};
string(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

eq(<<>> = Value, _Args, _) ->
    {ok, Value};
eq(Value, [Equivalent], AllData) ->
    eq(Value, [Equivalent], AllData);
eq(Value, Value, _) ->
    {ok, Value};
eq(Value, Equivalent, _) when is_float(Value) and is_binary(Equivalent) ->
    case float_to_binary(Value) of
        Equivalent -> {ok, Equivalent};
        _ -> {error, ?NOT_ALLOWED_VALUE}
    end;
eq(Value, Equivalent, _) when is_integer(Value) and is_binary(Equivalent) ->
    case integer_to_binary(Value) of
        Equivalent -> {ok, Equivalent};
        _ -> {error, ?NOT_ALLOWED_VALUE}
    end;
eq(Value, Equivalent, _) when is_binary(Value) and is_float(Equivalent) ->
    case float_to_binary(Equivalent) of
        Value -> {ok, Equivalent};
        _ -> {error, ?NOT_ALLOWED_VALUE}
    end;
eq(Value, Equivalent, _) when is_binary(Value) and is_integer(Equivalent) ->
    case integer_to_binary(Equivalent) of
        Value -> {ok, Equivalent};
        _ -> {error, ?NOT_ALLOWED_VALUE}
    end;
eq(Value, Equivalent, _) when is_binary(Value) and is_binary(Equivalent) ->
    {error, ?NOT_ALLOWED_VALUE};
eq(Value, Equivalent, _) when is_float(Value) and is_float(Equivalent) ->
    {error, ?NOT_ALLOWED_VALUE};
eq(Value, Equivalent, _) when is_integer(Value) and is_integer(Equivalent) ->
    {error, ?NOT_ALLOWED_VALUE};
eq(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

one_of(<<>> = Value, _Args, _) ->
    {ok, Value};
one_of(Value, Args, _) when is_binary(Value) ->
    ArgsList = lists:flatten(Args),
    case one_of_impl(Value, ArgsList) of
        {ok, Value} -> {ok, Value};
        {error, _} -> one_of_impl(binary_to_number(Value), ArgsList)
    end;
one_of(Value, Args, _Other) when is_float(Value) ->
    ArgsList = lists:flatten(Args),
    case one_of_impl(Value, ArgsList) of
        {ok, Value} -> {ok, Value};
        {error, _} -> one_of_impl(float_to_binary(Value), ArgsList)
    end;
one_of(Value, Args, _Other) when is_integer(Value) ->
    ArgsList = lists:flatten(Args),
    case one_of_impl(Value, ArgsList) of
        {ok, Value} -> {ok, Value};
        {error, _} -> one_of_impl(integer_to_binary(Value), ArgsList)
    end;
one_of(Value, Args, _) when is_boolean(Value) ->
    case lists:member(Value, lists:flatten(Args)) of
        true -> {ok, Value};
        false -> {error, ?NOT_ALLOWED_VALUE}
    end;
one_of(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

max_length(<<>> = Value, _Args, _) ->
    {ok, Value};
max_length(Value, [MaxLength], AllData) ->
    max_length(Value, MaxLength, AllData);
max_length(Value, MaxLength, _) when is_binary(Value) ->
    case length(unicode:characters_to_list(Value)) > MaxLength of
        true -> {error, ?TOO_LONG};
        false -> {ok, Value}
    end;
max_length(Value, MaxLength, AllData) when is_float(Value) ->
    max_length(float_to_binary(Value), MaxLength, AllData);
max_length(Value, MaxLength, AllData) when is_integer(Value) ->
    max_length(integer_to_binary(Value), MaxLength, AllData);
max_length(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

min_length(<<>> = Value, _Args, _) ->
    {ok, Value};
min_length(Value, [MinLength], AllData) ->
    min_length(Value, MinLength, AllData);
min_length(Value, MinLength, _) when is_binary(Value) ->
    case length(unicode:characters_to_list(Value)) < MinLength of
        true -> {error, ?TOO_SHORT};
        false -> {ok, Value}
    end;
min_length(Value, MaxLength, AllData) when is_float(Value) ->
    min_length(float_to_binary(Value), MaxLength, AllData);
min_length(Value, MaxLength, AllData) when is_integer(Value) ->
    min_length(integer_to_binary(Value), MaxLength, AllData);
min_length(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

length_between(<<>> = Value, _Args, _) ->
    {ok, Value};
length_between(Value, [[MinLength, MaxLength]], AllData) ->
    length_between(Value, [MinLength, MaxLength], AllData);
length_between(Value, [MinLength, MaxLength], _) when is_binary(Value) ->
    ValueLength = length(unicode:characters_to_list(Value)),
    if
        ValueLength > MaxLength -> {error, ?TOO_LONG};
        ValueLength < MinLength -> {error, ?TOO_SHORT};
        true -> {ok, Value}
    end;
length_between(Value, [MinLength, MaxLength], AllData) when is_float(Value) ->
    length_between(float_to_binary(Value), [MinLength, MaxLength], AllData);
length_between(Value, [MinLength, MaxLength], AllData) when is_integer(Value) ->
    length_between(integer_to_binary(Value), [MinLength, MaxLength], AllData);
length_between(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

length_equal(<<>> = Value, _Args, _) ->
    {ok, Value};
length_equal(Value, [Length], AllData) ->
    length_equal(Value, Length, AllData);
length_equal(Value, Length, _) when is_binary(Value) ->
    ValueLength = length(unicode:characters_to_list(Value)),
    if
        ValueLength < Length -> {error, ?TOO_SHORT};
        ValueLength > Length -> {error, ?TOO_LONG};
        ValueLength == Length -> {ok, Value}
    end;
length_equal(Value, Length, AllData) when is_float(Value) ->
    length_equal(float_to_binary(Value), Length, AllData);
length_equal(Value, Length, AllData) when is_integer(Value) ->
    length_equal(integer_to_binary(Value), Length, AllData);
length_equal(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

like(<<>> = Value, _Args, _) ->
    {ok, Value};
like(Value, [Pattern], AllData) ->
    like(Value, Pattern, AllData);
like(Value, [Pattern, <<"i">>], _) when is_binary(Value) ->
    like_impl(Value, Pattern, [unicode, caseless]);
like(Value, [Pattern, <<"i">>], _) when is_float(Value) ->
    like_impl(float_to_binary(Value), Pattern, [unicode, caseless]);
like(Value, [Pattern, <<"i">>], _) when is_integer(Value) ->
    like_impl(integer_to_binary(Value), Pattern, [unicode, caseless]);
like(Value, Pattern, _) when is_binary(Value) ->
    like_impl(Value, Pattern, [unicode]);
like(Value, Pattern, _) when is_float(Value) ->
    like_impl(float_to_binary(Value), Pattern, [unicode]);
like(Value, Pattern, _) when is_integer(Value) ->
    like_impl(integer_to_binary(Value), Pattern, [unicode]);
like(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% INTERNAL
like_impl(Value, Pattern, Opts) ->
    case re:compile(Pattern, Opts) of
        {ok, MP} ->
            case re:run(Value, MP) of
                nomatch -> {error, ?WRONG_FORMAT};
                _ -> {ok, Value}
            end;
        {error, _} -> {error, ?FORMAT_ERROR}
    end.

one_of_impl(Value, ArgsList) ->
    case lists:member(Value, ArgsList) of
        true -> {ok, Value};
        false -> {error, ?NOT_ALLOWED_VALUE}
    end.

binary_to_number(BinValue) ->
    try
        binary_to_integer(BinValue)
    catch
        _:_ ->
            try
                binary_to_float(BinValue)
            catch
                _:_ ->
                    BinValue
            end

    end.