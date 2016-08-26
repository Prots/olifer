-module(olifer_commons).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([
    required/3,
    not_empty/3,
    not_empty_list/3,
    any_object/3
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

any_object_impl([], Value) ->
    {ok, Value};
any_object_impl([Object|Rest], Value) when is_tuple(Object) ->
    any_object_impl(Rest, Value);
any_object_impl([_|_], _) ->
    {error, ?FORMAT_ERROR}.
