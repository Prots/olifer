-module(olifer_lists).

-include("olifer.hrl").

%% API
-export([
    list_length/3,
    unique/3
]).
%% For the schema check
-export([length_bounds/1]).

%% API
%% {"list_length": 2}, {"list_length": [1, 10]} or {"list_length": {"min": 1, "max": 10}}
list_length(Value, _Args, _) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
list_length(Value, Args, _) when is_list(Value) ->
    {ok, Min, Max} = length_bounds(olifer:args(Args)),
    Length = length(Value),
    case olifer:is_object(Value) of
        true -> {error, ?FORMAT_ERROR};
        false when Length < Min -> {error, ?TOO_FEW_ITEMS};
        false when Max =/= infinity, Length > Max -> {error, ?TOO_MANY_ITEMS};
        false -> {ok, Value}
    end;
list_length(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% "unique": primitive items, compared as strings; {"unique": ["country", "zip"]}: objects,
%% compared by the values of the keys. Empty items are ignored
unique(Value, _Args, _) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
unique(Value, Args, _) when is_list(Value) ->
    case olifer:is_object(Value) of
        true ->
            {error, ?FORMAT_ERROR};
        false ->
            Keys = [olifer_schema:string_arg(Key) || Key <- olifer:args(Args)],
            case unique_impl(Value, Keys, #{}, false) of
                ok -> {ok, Value};
                Error -> {error, Error}
            end
    end;
unique(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% The arguments of list_length as {ok, Min, Max} (Max may be infinity), or error.
%% An object first is the options form, otherwise one argument is the exact length and
%% two or more are min and max
length_bounds([[{}] | _]) ->
    error;
length_bounds([Options | _] = Args) ->
    case olifer:is_object(Options) of
        true ->
            case [Key || {Key, _} <- Options, Key =/= <<"min">>, Key =/= <<"max">>] of
                [] -> bounds(option(<<"min">>, Options, 0), option(<<"max">>, Options, infinity));
                _ -> error
            end;
        false ->
            case Args of
                [Length] -> bounds(Length, Length);
                [Min, Max | _] -> bounds(Min, Max)
            end
    end;
length_bounds([]) ->
    error.

%% INTERNAL
option(Name, Options, Default) ->
    case olifer:get_value(Name, Options) of
        undefined -> Default;
        Value -> Value
    end.

bounds(Min, Max) ->
    case {olifer_schema:non_negative_integer(Min), Max} of
        {error, _} -> error;
        {MinLength, infinity} -> {ok, MinLength, infinity};
        {MinLength, _} ->
            case olifer_schema:non_negative_integer(Max) of
                error -> error;
                MaxLength -> {ok, MinLength, MaxLength}
            end
    end.

%% A wrong item gives FORMAT_ERROR, even after duplicates
unique_impl([], _Keys, _Seen, false) ->
    ok;
unique_impl([], _Keys, _Seen, true) ->
    ?NOT_UNIQUE_ITEMS;
unique_impl([Item | Rest], Keys, Seen, HasDuplicates) ->
    case item_key(Item, Keys) of
        empty -> unique_impl(Rest, Keys, Seen, HasDuplicates);
        error -> ?FORMAT_ERROR;
        Key -> unique_impl(Rest, Keys, Seen#{Key => true}, HasDuplicates orelse maps:is_key(Key, Seen))
    end.

%% The item as a string, or the values of the keys as strings; empty when the item or
%% a value of a key is empty
item_key(Item, Keys) ->
    case olifer:is_empty(Item) of
        true -> empty;
        false when Keys =:= [] ->
            case olifer:to_binary(Item) of
                undefined -> error;
                String -> String
            end;
        false ->
            case olifer:is_object(Item) of
                true -> key_values([olifer:get_value(Key, Item) || Key <- Keys], []);
                false -> error
            end
    end.

key_values([], Acc) ->
    lists:reverse(Acc);
key_values([Value | Rest], Acc) ->
    case {olifer:is_empty(Value), olifer:to_binary(Value)} of
        {true, _} ->
            %% an object or a list in another key is still an error
            case [V || V <- Rest, not olifer:is_empty(V), olifer:to_binary(V) =:= undefined] of
                [] -> empty;
                _ -> error
            end;
        {false, undefined} -> error;
        {false, String} -> key_values(Rest, [String | Acc])
    end.
