-module(olifer_commons).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([
    required/3,
    not_empty/3,
    not_empty_list/3,
    any_object/3,
    boolean/3,
    required_if/3,
    any/3
]).

%% API
required(<<>>, [], _) ->
    {error, ?REQUIRED};
required(null, [], _) ->
    {error, ?REQUIRED};
required(undefined, [], _) ->
    {error, ?REQUIRED};
required(Value, [], _) ->
    {ok, Value}.

not_empty(<<>>, [], _) ->
    {error, ?CANNOT_BE_EMPTY};
not_empty(Value, [], _) ->
    {ok, Value}.

not_empty_list(<<>>, _Args, _) ->
    {error, ?CANNOT_BE_EMPTY};
not_empty_list([], _Args, _) ->
    {error, ?CANNOT_BE_EMPTY};
not_empty_list([{}], [], _) ->
    {error, ?FORMAT_ERROR};
not_empty_list(Value, [], _) when is_list(Value) ->
    {ok, Value};
not_empty_list(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

any_object(<<>> = Value, _Args, _) ->
    {ok, Value};
any_object([{}] = Value, _Args, _) ->
    {ok, Value};
any_object([], _Args, _) ->
    {error, ?FORMAT_ERROR};
any_object(Value, _Args, _) when is_list(Value)->
    any_object_impl(Value, Value);
any_object(Value, _Args, _) when is_tuple(Value) ->
    {ok, Value};
any_object(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% true, false, 1, 0, "1", "0", "true" and "false", the output is a boolean
boolean(Value, _Args, _) when Value =:= <<>>; Value =:= null; is_boolean(Value) ->
    {ok, Value};
boolean(Value, _Args, _) when Value =:= <<"1">>; Value =:= <<"true">>; is_number(Value), Value == 1 ->
    {filter, true};
boolean(Value, _Args, _) when Value =:= <<"0">>; Value =:= <<"false">>; is_number(Value), Value == 0 ->
    {filter, false};
boolean(Value, _Args, _) when is_binary(Value); is_number(Value) ->
    {error, ?NOT_BOOLEAN};
boolean(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% {"required_if": {"type": "company", "address/country": ["UA", "PL"]}}: the field is required
%% when every referenced input value is not empty and equals the value or one of the values
required_if(Value, Args, AllData) ->
    [Conditions | _] = olifer:args(Args),
    IsMatch = fun({Reference, Expected}) -> is_condition_match(Reference, Expected, AllData) end,
    case olifer:is_empty(Value) andalso lists:all(IsMatch, Conditions) of
        true -> {error, ?REQUIRED};
        false -> {ok, Value}
    end.

%% Any value, the output is the value as it is
any(Value, _Args, _) ->
    {ok, Value}.

%% INTERNAL
is_condition_match(Reference, Expected, AllData) ->
    {ok, Path} = olifer:parse_reference(Reference),
    Value = olifer:reference_value(Path, AllData),
    ExpectedValues = case is_list(Expected) of
        true -> Expected;
        false -> [Expected]
    end,
    olifer:is_primitive(Value) andalso Value =/= <<>>
        andalso lists:member(olifer:to_binary(Value), [olifer:to_binary(E) || E <- ExpectedValues]).

any_object_impl([], Value) ->
    {ok, Value};
any_object_impl([Object|Rest], Value) when is_tuple(Object) ->
    any_object_impl(Rest, Value);
any_object_impl([_|_], _) ->
    {error, ?FORMAT_ERROR}.
