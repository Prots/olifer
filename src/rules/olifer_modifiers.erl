-module(olifer_modifiers).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% API
-export([
    trim/3,
    to_uc/3,
    to_lc/3,
    remove/3,
    leave_only/3,
    default/3
]).

%% API
trim(Value, [], _) when is_binary(Value) ->
    {filter, bstring:trim(Value)};
trim(Value, [], _) ->
    {filter, Value}.

to_lc(Value, [], _) when is_binary(Value) ->
    {filter, bstring:to_lower(Value)};
to_lc(Value, [], _) ->
    {filter, Value}.

to_uc(Value, [], _) when is_binary(Value) ->
    {filter, bstring:to_upper(Value)};
to_uc(Value, [], _) ->
    {filter, Value}.

remove(Value, [Pattern], AllData) ->
    remove(Value, Pattern, AllData);
remove(Value, Pattern, _) when is_binary(Value), is_binary(Pattern) ->
    remove_impl(Value, Pattern);
remove(Value, _, _) ->
    {filter, Value}.

leave_only(Value, [Pattern], AllData) ->
    leave_only(Value, Pattern, AllData);
leave_only(Value, Pattern, _)  when is_binary(Value), is_binary(Pattern)->
    leave_only_impl(Value, Pattern, <<>>);
leave_only(Value, _, _) ->
    {filter, Value}.

default(<<>>, [[]], _AllData) ->
    {filter, []};
default(<<>>, [{}], _AllData) ->
    {filter, [{}]};
default(<<>>, [Default], _AllData) ->
    {filter, Default};
default(<<>>, Default, _AllData) ->
    {filter, Default};
default(Value, _Default, _AllData) ->
    {filter, Value}.

%% INTERNAL
remove_impl(Value, <<>>) ->
    {filter, Value};
remove_impl(Value, <<First:1/binary, Rest/binary>>) ->
    NewValueList = bstring:split_global(Value, First),
    remove_impl(binary:list_to_bin(NewValueList), Rest).

leave_only_impl(<<>>, _, Acc) ->
    {filter, Acc};
leave_only_impl(<<First:1/binary, Rest/binary>>, Pattern, Acc) ->
    case binary:match(Pattern, First) of
        nomatch -> leave_only_impl(Rest, Pattern, Acc);
        _ -> leave_only_impl(Rest, Pattern, <<Acc/binary, First/binary>>)
    end.
