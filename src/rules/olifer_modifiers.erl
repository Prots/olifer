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
trim(Value, [], _) ->
    modify_string(Value, fun(String) ->
        re:replace(String, "^[" ?WHITESPACE "]+|[" ?WHITESPACE "]+$", "",
                   [unicode, global, dollar_endonly, {return, binary}])
    end).

to_lc(Value, [], _) ->
    modify_string(Value, fun string:lowercase/1).

to_uc(Value, [], _) ->
    modify_string(Value, fun string:uppercase/1).

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

default(null, Default, AllData) ->
    default(<<>>, Default, AllData);
default(<<>>, [[]], _AllData) ->
    {filter, []};
default(<<>>, [{}], _AllData) ->
    {filter, [{}]};
default(<<>>, [{_, _}] = Default, _AllData) ->
    {filter, Default};
default(<<>>, [Default], _AllData) ->
    {filter, Default};
default(<<>>, Default, _AllData) ->
    {filter, Default};
default(Value, _Default, _AllData) ->
    {filter, Value}.

%% INTERNAL
%% Strings, numbers and booleans are modified as strings, other values
%% (and binaries that are not valid UTF-8) are left as they are
modify_string(Value, Modify) ->
    case olifer:to_binary(Value) of
        undefined ->
            {filter, Value};
        String ->
            case unicode:characters_to_binary(String) of
                String -> {filter, Modify(String)};
                _ -> {filter, Value}
            end
    end.

remove_impl(Value, <<>>) ->
    {filter, Value};
remove_impl(Value, Pattern) ->
    {First, Rest} = next_char(Pattern),
    remove_impl(binary:replace(Value, First, <<>>, [global]), Rest).

leave_only_impl(<<>>, _, Acc) ->
    {filter, Acc};
leave_only_impl(Value, Pattern, Acc) ->
    {First, Rest} = next_char(Value),
    case binary:match(Pattern, First) of
        nomatch -> leave_only_impl(Rest, Pattern, Acc);
        _ -> leave_only_impl(Rest, Pattern, <<Acc/binary, First/binary>>)
    end.

%% The first UTF-8 character, or the first byte if it is not valid UTF-8
next_char(<<Char/utf8, Rest/binary>>) ->
    {<<Char/utf8>>, Rest};
next_char(<<Byte, Rest/binary>>) ->
    {<<Byte>>, Rest}.
