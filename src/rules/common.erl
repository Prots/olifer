-module(common).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([required/3]).
-export([not_empty/3]).
-export([not_empty_list/3]).

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
    {error, ?WRONG_FORMAT};
not_empty_list(Value, [], _) when is_list(Value) ->
    {ok, Value};
not_empty_list(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.
