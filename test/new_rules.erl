-module(new_rules).
-author("prots.igor@gmail.com").

-export([strong_password/2]).

strong_password(Value, [MinLength]) ->
    strong_password(Value, MinLength);
strong_password(Value, MinLength) when is_binary(Value), byte_size(Value) >= MinLength ->
    {ok, Value};
strong_password(Value, _Args) when is_binary(Value) ->
    {error, <<"WEAK_PSSSWORD">>};
strong_password(_Value, _Args) ->
    {error, <<"FORMAT_ERROR">>}.

