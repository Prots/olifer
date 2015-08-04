-module(strings).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([one_of/3]).
-export([max_length/3]).
-export([min_length/3]).
-export([length_between/3]).
-export([length_equal/3]).
-export([like/3]).

%% API
one_of(<<>> = Value, _Args, _) ->
    {ok, Value};
one_of(Value, Args, _) when is_binary(Value) ->
    case lists:member(Value, lists:flatten(Args)) of
        true -> {ok, Value};
        false -> {error, ?NOT_ALLOWED_VALUE}
    end;
one_of(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

max_length(<<>> = Value, _Args, _) ->
    {ok, Value};
max_length(Value, [MaxLength], AllData) when is_binary(Value) ->
    max_length(Value, MaxLength, AllData);
max_length(Value, MaxLength, _) when is_binary(Value) ->
    case length(unicode:characters_to_list(Value)) > MaxLength of
        true -> {error, ?TOO_LONG};
        false -> {ok, Value}
    end;
max_length(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

min_length(<<>> = Value, _Args, _) ->
    {ok, Value};
min_length(Value, [MinLength], AllData) when is_binary(Value) ->
    min_length(Value, MinLength, AllData);
min_length(Value, MinLength, _) when is_binary(Value) ->
    case length(unicode:characters_to_list(Value)) < MinLength of
        true -> {error, ?TOO_SHORT};
        false -> {ok, Value}
    end;
min_length(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

length_between(<<>> = Value, _Args, _) ->
    {ok, Value};
length_between(Value, [[MinLength, MaxLength]], AllData) when is_binary(Value) ->
    length_between(Value, [MinLength, MaxLength], AllData);
length_between(Value, [MinLength, MaxLength], _) when is_binary(Value) ->
    ValueLength = length(unicode:characters_to_list(Value)),
    if
        ValueLength > MaxLength -> {error, ?TOO_LONG};
        ValueLength < MinLength -> {error, ?TOO_SHORT};
        true -> {ok, Value}
    end;
length_between(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

length_equal(<<>> = Value, _Args, _) ->
    {ok, Value};
length_equal(Value, [Length], AllData) when is_binary(Value) ->
    length_equal(Value, Length, AllData);
length_equal(Value, Length, _) when is_binary(Value) ->
    ValueLength = length(unicode:characters_to_list(Value)),
    if
        ValueLength < Length -> {error, ?TOO_SHORT};
        ValueLength > Length -> {error, ?TOO_LONG};
        ValueLength == Length -> {ok, Value}
    end;
length_equal(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

like(<<>> = Value, _Args, _) ->
    {ok, Value};
like(Value, [Pattern], AllData) when is_binary(Value) ->
    like(Value, Pattern, AllData);
like(Value, [Pattern, <<"i">>], _) when is_binary(Value) ->
    like_impl(Value, Pattern, [unicode, caseless]);
like(Value, Pattern, _) when is_binary(Value) ->
    like_impl(Value, Pattern, [unicode]);
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