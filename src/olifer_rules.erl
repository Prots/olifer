-module(olifer_rules).
-author("prots.igor@gmail.com").

-include("olifer.hrl").

%% BASIC RULES
-export([required/3]).
-export([not_empty/3]).
-export([not_empty_list/3]).
%% NUMBER RULES
-export([integer/3]).
-export([positive_integer/3]).
-export([decimal/3]).
-export([positive_decimal/3]).
-export([max_number/3]).
-export([min_number/3]).
-export([number_between/3]).
%% STRING RULES
-export([one_of/3]).
-export([max_length/3]).
-export([min_length/3]).
-export([length_between/3]).
-export([length_equal/3]).
-export([like/3]).
%% SPECIAL RULES
-export([email/3]).
%% -export([url/3]).
%% -export([iso_date/3]).
-export([equal_to_field/3]).
-export([nested_object/3]).
-export([list_of/3]).
-export([list_of_objects/3]).

%% BASIC RULES
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

not_empty_list(_Value, [], _) ->
    true.

%% NUMBER RULES
integer(<<>> = Value, [], _) ->
    {ok, Value};
integer(Value, [], _) when is_integer(Value) ->
    {ok, Value};
integer(Value, [], _) when is_binary(Value) ->
    case binary_to_int(Value) of
        error -> {error, ?NOT_INTEGER};
        {ok, _} -> {ok, Value}
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
        {ok, IntValue} when IntValue > 0 -> {ok, Value};
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
        {ok, _} -> {ok, Value}
    end;
decimal(Value, [], _) when is_integer(Value) ->
    {error, ?NOT_DECIMAL};
decimal(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

positive_decimal(<<>> = Value, [], _) ->
    {ok, Value};
positive_decimal(Value, [], _) when is_float(Value), Value > 0 ->
    {ok, Value};
positive_decimal(Value, [], _) when is_float(Value), Value =< 0 ->
    {error, ?NOT_POSITIVE_DECIMAL};
positive_decimal(Value, [], _) when is_binary(Value) ->
    case binary_to_flt(Value) of
        error -> {error, ?NOT_POSITIVE_DECIMAL};
        {ok, FltValue} when FltValue > 0 -> {ok, Value};
        _ -> {error, ?NOT_POSITIVE_DECIMAL}
    end;
positive_decimal(Value, [], _) when is_integer(Value) ->
    {error, ?NOT_POSITIVE_DECIMAL};
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
        {error, error} -> ?FORMAT_ERROR;
        {{ok, IntValue}, _} when IntValue =< MaxNumber -> {ok, Value};
        {_, {ok, FltValue}} when FltValue =< MaxNumber -> {ok, Value};
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
        {error, error} -> ?FORMAT_ERROR;
        {{ok, IntValue}, _} when IntValue >= MinNumber -> {ok, Value};
        {_, {ok, FltValue}} when FltValue >= MinNumber -> {ok, Value};
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
        {error, error} -> ?FORMAT_ERROR;
        {{ok, IntValue}, _} when IntValue > MaxNumber -> {error, ?TOO_HIGH};
        {{ok, IntValue}, _} when IntValue < MinNumber -> {error, ?TOO_LOW};
        {_, {ok, FltValue}} when FltValue > MaxNumber -> {error, ?TOO_HIGH};
        {_, {ok, FltValue}} when FltValue < MinNumber -> {error, ?TOO_LOW};
        _ -> {ok, Value}
    end;
number_between(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

%% STRING RULES
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

%% SPECIAL RULES
email(<<>> = Value, _Args, _) ->
    {ok, Value};
email(Value, [], _) when is_binary(Value) ->
    Pattern = "^((?:(?:[^\"@\\.\s]+\\.?)|(?:\\.\"[^\"\s]+\"\\.))*(?:(?:\\.?\"[^\"\s]+\")|(?:[a-zA-Z0-9\\-_]+)))@[a-z0-9\\.\\-\\[\\]]+$",
    case re:run(Value, Pattern, [caseless, anchored]) of
        nomatch -> {error, ?WRONG_EMAIL};
        _ -> {ok, Value}
    end;
email(_, _, _) ->
    {error, ?FORMAT_ERROR}.

equal_to_field(<<>> = Value, _Args, _) ->
    {ok, Value};
equal_to_field(Value, [FieldName], AllData) when is_binary(Value); is_integer(Value); is_float(Value) ->
    equal_to_field(Value, FieldName, AllData);
equal_to_field(Value, FieldName, AllData) when is_binary(Value); is_integer(Value); is_float(Value) ->
    FieldValue = proplists:get_value(FieldName, AllData),
    case Value =:= FieldValue of
        true -> {ok, Value};
        false -> {error, ?FIELDS_NOT_EQUAL}
    end;
equal_to_field(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

nested_object(<<>> = Value, _Args, _) ->
    {ok, Value};
nested_object(Value, Args, _AllData) when is_list(Value)->
    FieldsList = olifer:validate(Value, Args),
    post_processing(nested_object, FieldsList);
nested_object(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.

list_of(<<>> = Value, _Args, _) ->
    {ok, Value};
list_of(Values, [Args], AllData) when is_list(Values) ->
    list_of(Values, Args, AllData);
list_of(Values, Args, _) when is_list(Values) ->
    {DataPropList, RulesPropList} = pred_processing(list_of, Values, Args),
    FieldsList = olifer:validate(DataPropList, RulesPropList),
    post_processing(list_of, FieldsList);
list_of(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.


list_of_objects(<<>> = Value, _Args, _) ->
    {ok, Value};
list_of_objects(Values, [Args], AllData) when is_list(Values) ->
    list_of(Values, Args, AllData);
list_of_objects(Values, Args, _) when is_list(Values) ->
    {DataPropList, RulesPropList} = pred_processing(list_of, Values, Args),
    FieldsList = olifer:validate(DataPropList, RulesPropList),
    post_processing(list_of, FieldsList);
list_of_objects(_Value, _Args, _) ->
    {error, ?FORMAT_ERROR}.


%% INTERNAL
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
                        FltValue1 = binary_to_float(<<Value/binary, ".0">>),
                        {ok, FltValue1}
                    catch
                        _:_ -> error
                    end
            end;
        _ -> error
    end.

post_processing(nested_object, FieldsList) ->
    post_processing(nested_object, FieldsList, [], []);
post_processing(list_of, FieldsList) ->
    post_processing(list_of, FieldsList, [], []).

post_processing(nested_object, [], AccOK, []) ->
    Result = [{Field#field.name, Field#field.output} || Field <- AccOK],
    {ok, lists:reverse(Result)};
post_processing(nested_object, [], _AccOK, AccErr) ->
    Result = [{Field#field.name, Field#field.errors} || Field <- AccErr],
    {error, lists:reverse(Result)};
post_processing(nested_object, [#field{errors = []} = Field|RestList], AccOK, AccErr) ->
    post_processing(nested_object, RestList, [Field|AccOK], AccErr);
post_processing(nested_object, [#field{errors = _Err} = Field|RestList], AccOK, AccErr) ->
    post_processing(nested_object, RestList, AccOK, [Field|AccErr]);
post_processing(list_of, [], AccOk, AccErr) when length(AccErr) == length(AccOk) ->
    {ok, AccOk};
post_processing(list_of, [], _AccOk, AccErr) ->
    {error, AccErr};
post_processing(list_of, [#field{output = Res, errors = []}|RestList], AccOK, AccErr) ->
    post_processing(list_of, RestList, [Res|AccOK], [null|AccErr]);
post_processing(list_of, [#field{errors = Err}|RestList], AccOK, AccErr) ->
    post_processing(list_of, RestList, AccOK, [Err|AccErr]).

pred_processing(list_of, Values, Args) ->
    pred_processing(list_of, Values, Args, [], []).

pred_processing(list_of, [], _, DataPropList, RulesPropList) ->
    {DataPropList, RulesPropList};
pred_processing(list_of, [Value|Rest], Args, DataList, RulesList) ->
    TempFieldName = erlang:make_ref(),
    pred_processing(list_of, Rest, Args, [{TempFieldName, Value}|DataList], [{TempFieldName, Args}|RulesList]).


like_impl(Value, Pattern, Opts) ->
    case re:compile(Pattern, Opts) of
        {ok, MP} ->
            case re:run(Value, MP) of
                nomatch -> {error, ?WRONG_FORMAT};
                _ -> {ok, Value}
            end;
        {error, _} -> {error, ?FORMAT_ERROR}
    end.