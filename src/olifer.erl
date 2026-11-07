-module(olifer).

-include("olifer.hrl").

%% LIVR API
-export([validate/2]).
-export([check_schema/1]).
-export([register_rule/3]).
-export([register_aliased_rule/1]).
%% FOR INTERNAL USAGE
-export([start/0, stop/0]).
-export([validate_data/2]).
-export([validate_value/2, validate_value/3]).
-export([get_value/2]).
-export([to_binary/1]).
-export([is_empty/1, is_primitive/1, is_object/1, args/1]).
-export([parse_reference/1, reference_value/2]).
-export([rule_kind/1]).

%% LIVR API
validate([], []) ->
    {ok, [{}]};
validate([{}], []) ->
    {ok, [{}]};
validate(_Data, [{}]) ->
    {ok, [{}]};
validate(Data, Rules) when is_list(Data), is_list(Rules) ->
    ok = check_schema_cached(Rules),
    FieldsList = validate_data(Data, Rules),
    {Type, Result} = return_result(FieldsList, [], []),
    {Type, lists:reverse(Result)};
validate(Data, _Rules) ->
    [{Data, ?FORMAT_ERROR}].

%% The schema check of LIVR 2.5, without validating data: ok, or {error, Message} for an unknown
%% rule (or alias), a wrong argument of a LIVR 2.5 rule, or an empty list of rules or null in
%% place of rules in "when", "values" or "keys". validate/2 raises error({schema_error, Message})
%% for the same errors
check_schema(Rules) ->
    try olifer_schema:check(Rules) of
        ok -> ok
    catch
        error:{schema_error, Message} -> {error, Message}
    end.

register_rule(Name, Module, Function) when is_atom(Module), is_atom(Function) ->
    true = ets:insert(?RULES_TBL, {Name, Module, Function}),
    true = ets:delete_all_objects(?SCHEMAS_TBL),
    ok.

register_aliased_rule(Aliases) when is_list(Aliases) ->
    true = ets:delete_all_objects(?SCHEMAS_TBL),
    register_aliases(Aliases).

%% FOR INTERNAL USAGE
get_value(Key, List) when is_list(List) ->
    get_value(Key, List, undefined);
get_value(_, _) ->
    undefined.

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_, Val} -> Val;
        _ -> Default
    end.

%% Primitive value (string, number or boolean) as a string, like JavaScript's String() does it.
%% Returns undefined for other values (objects, lists, null).
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_float(Value) ->
    list_to_binary(float_to_string(Value));
to_binary(Value) when is_boolean(Value) ->
    atom_to_binary(Value, utf8);
to_binary(_Value) ->
    undefined.

%% A missing field, null or ""
is_empty(<<>>) -> true;
is_empty(null) -> true;
is_empty(undefined) -> true;
is_empty(_Value) -> false.

%% A string, a number or a boolean
is_primitive(Value) ->
    is_binary(Value) orelse is_number(Value) orelse is_boolean(Value).

%% A JSON object: a proplist, [{}] is an empty object ([] is an empty list)
is_object([{}]) -> true;
is_object([{_, _} | _]) -> true;
is_object(_Value) -> false.

%% The arguments of a rule as a list: {"rule": "a"} and {"rule": {"min": 1}} have one argument,
%% {"rule": ["a", "b"]} has two
args(Args) when is_list(Args) ->
    case is_object(Args) of
        true -> [Args];
        false -> Args
    end;
args(Arg) ->
    [Arg].

%% A reference to another field (LIVR 2.5): "name", or a path "a/b/0" down from the containing
%% object, with "~1" for "/" and "~0" for "~" in a segment, as in JSON Pointer
parse_reference(<<>>) ->
    error;
parse_reference(Reference) when is_binary(Reference) ->
    Segments = binary:split(Reference, <<"/">>, [global]),
    case lists:member(<<>>, Segments) of
        true -> error;
        false -> unescape_segments(Segments, [])
    end;
parse_reference(_Reference) ->
    error.

%% The input value at the path, undefined when a segment is missing or a value on the way
%% is not an object (or a list for an index segment)
reference_value([], Value) ->
    Value;
reference_value([Segment | Rest], Value) when is_list(Value) ->
    case is_object(Value) of
        true -> reference_value(Rest, get_value(Segment, Value));
        false -> reference_value(Rest, list_item(Segment, Value))
    end;
reference_value(_Path, _Value) ->
    undefined.

%% How a rule name is defined: builtin, {alias, Rules}, user (a registered rule) or undefined
rule_kind(Name) ->
    case rule_to_atom(Name) of
        {_, _} ->
            builtin;
        undefined ->
            case {lookup_alias(Name), lookup_rules(Name)} of
                {{Name, Rules, _}, _} -> {alias, Rules};
                {undefined, {Name, _, _}} -> user;
                {undefined, undefined} -> undefined
            end
    end.

start() ->
    start(?MODULE).

stop() ->
    application:stop(?MODULE).

validate_data(DataPropList, RulesPropList) when is_list(DataPropList), is_list(RulesPropList) ->
    ListOfFields = prepare(DataPropList, RulesPropList),
    lists:reverse([Field || Field <- [apply_rules(F, DataPropList) || F <- ListOfFields],
                            not is_still_missing(Field)]);
validate_data(Data, Rules) ->
    [#field{name = Data, input = Data, rules = Rules, output = ?FORMAT_ERROR, errors = ?FORMAT_ERROR}].

%% Applies the rules to one value, for meta rules like "or".
%% HasValue tells whether a rule gave the value an output (a modifier like "default" always does).
validate_value(Value, Rules) ->
    validate_value(Value, Rules, [{}]).

%% AllData is the containing object: the rules of "or" and "when" see the fields next to the
%% validated one
validate_value(Value, Rules, AllData) ->
    Field = #field{name = value, input = Value, rules = Rules, output = Value, errors = [], missing = true},
    case apply_rules(Field, AllData) of
        #field{errors = [], output = Output, missing = Missing} -> {ok, Output, not Missing};
        #field{errors = Errors} -> {error, Errors}
    end.

%% INTERNAL
%% The schema check of validate/2. The schemas that passed it are kept (up to ?MAX_SCHEMAS), so
%% that a schema is checked once, not on every validation, until a rule or an alias is registered
check_schema_cached(Rules) ->
    case ets:member(?SCHEMAS_TBL, Rules) of
        true ->
            ok;
        false ->
            ok = olifer_schema:check(Rules),
            _ = ets:info(?SCHEMAS_TBL, size) < ?MAX_SCHEMAS andalso ets:insert(?SCHEMAS_TBL, {Rules}),
            ok
    end.

unescape_segments([], Acc) ->
    {ok, lists:reverse(Acc)};
unescape_segments([Segment | Rest], Acc) ->
    case unescape(Segment, <<>>) of
        error -> error;
        Unescaped -> unescape_segments(Rest, [Unescaped | Acc])
    end.

unescape(<<>>, Acc) -> Acc;
unescape(<<"~0", Rest/binary>>, Acc) -> unescape(Rest, <<Acc/binary, "~">>);
unescape(<<"~1", Rest/binary>>, Acc) -> unescape(Rest, <<Acc/binary, "/">>);
unescape(<<"~", _/binary>>, _Acc) -> error;
unescape(<<Byte, Rest/binary>>, Acc) -> unescape(Rest, <<Acc/binary, Byte>>).

%% An index segment is "0" or a number without leading zeros
list_item(<<"0">>, [First | _]) ->
    First;
list_item(<<First, _/binary>> = Segment, List) when First >= $1, First =< $9 ->
    try binary_to_integer(Segment) of
        Index when Index < length(List) -> lists:nth(Index + 1, List);
        _ -> undefined
    catch
        error:badarg -> undefined
    end;
list_item(_Segment, _List) ->
    undefined.

%% Number.prototype.toString() of JavaScript: the shortest digits that give the same float,
%% with an exponent below 1e-6 and from 1e21: 1.2 -> "1.2", 1.0e21 -> "1e+21", 1.0e-7 -> "1e-7"
float_to_string(Value) when Value == 0 ->
    "0";
float_to_string(Value) ->
    {Mantissa, Exp} = case string:split(io_lib_format:fwrite_g(abs(Value)), "e") of
        [M, E] -> {M, list_to_integer(E)};
        [M] -> {M, 0}
    end,
    [Int, Frac] = string:split(Mantissa, "."),
    %% Value = 0.Digits * 10^Point
    {Digits, Point} = strip_leading_zeros(Int ++ Frac, length(Int) + Exp),
    Sign = case Value < 0 of
        true -> "-";
        false -> ""
    end,
    Sign ++ format_digits(string:trim(Digits, trailing, "0"), Point).

strip_leading_zeros([$0 | Digits], Point) ->
    strip_leading_zeros(Digits, Point - 1);
strip_leading_zeros(Digits, Point) ->
    {Digits, Point}.

format_digits(Digits, Point) when length(Digits) =< Point, Point =< 21 ->
    Digits ++ lists:duplicate(Point - length(Digits), $0);
format_digits(Digits, Point) when 0 < Point, Point =< 21 ->
    {Int, Frac} = lists:split(Point, Digits),
    Int ++ "." ++ Frac;
format_digits(Digits, Point) when -6 < Point, Point =< 0 ->
    "0." ++ lists:duplicate(-Point, $0) ++ Digits;
format_digits([First | Rest], Point) ->
    Mantissa = case Rest of
        [] -> [First];
        _ -> [First, $. | Rest]
    end,
    ExpSign = case Point > 0 of
        true -> "+";
        false -> "-"
    end,
    Mantissa ++ "e" ++ ExpSign ++ integer_to_list(abs(Point - 1)).

return_result([], AccOk, []) ->
    {ok, AccOk};
return_result([], _, AccErr) ->
    {errors, AccErr};
return_result([#field{errors = []} = Field | Rest], AccOk, AccErr) ->
    return_result(Rest, [{Field#field.name, Field#field.output} | AccOk], AccErr);
return_result([Field | Rest], AccOk, AccErr) ->
    return_result(Rest, AccOk, [{Field#field.name, Field#field.errors} | AccErr]).

%% A missing field (it is validated when it has rules like "required" or "or")
%% stays missing, unless a rule (like "default") gives it a value
is_still_missing(#field{errors = [], missing = true}) ->
    true;
is_still_missing(_Field) ->
    false.

register_aliases([]) ->
    ok;
register_aliases([Alias | Rest]) ->
    Name = olifer:get_value(<<"name">>, Alias),
    Rules = olifer:get_value(<<"rules">>, Alias),
    ErrorCode = olifer:get_value(<<"error">>, Alias),
    true = ets:insert(?ALIASES_TBL, {Name, Rules, ErrorCode}),
    register_aliases(Rest).

lookup_alias(Name) ->
    case ets:lookup(?ALIASES_TBL, Name) of
        [] -> undefined;
        [{Name, Rules, Error} | _] -> {Name, Rules, Error}
    end.

lookup_rules(Name) ->
    case ets:lookup(?RULES_TBL, Name) of
        [] -> undefined;
        [{Name, Module, Function} | _] -> {Name, Module, Function}
    end.

prepare(DataPropList, RulesPropList) ->
    prepare(DataPropList, DataPropList, RulesPropList, []).

prepare(_DataPropList, _AllData, [], Acc) ->
    Acc;
prepare(_DataPropList, _AllData, [{}], Acc) ->
    Acc;
prepare(DataPropList, AllData, [{FieldName, FieldRules} | RestRules], Acc) ->
    case lists:keyfind(FieldName, 1, DataPropList) of
        false ->
            case has_spec_rule(FieldRules) of
                false ->
                    prepare(DataPropList, AllData, RestRules, Acc);
                SpecRulesList ->
                    prepare(DataPropList, AllData, RestRules, [#field{name = FieldName, input = <<>>, rules = SpecRulesList, missing = true} | Acc])
            end;
        {FieldName, FieldData} ->
            prepare(DataPropList, AllData, RestRules, [#field{name = FieldName, input = FieldData, rules = FieldRules} | Acc])
    end.

apply_rules(#field{rules = []} = Field, _AllData) ->
    Field;
apply_rules(#field{rules = {Rule, Args}} = Field, AllData) ->
    {NewInp, Output, Errors, HasValue} = apply_one_rule(Field, {Rule, Args}, AllData),
    apply_rules(Field#field{input = NewInp, rules = [], output = Output, errors = Errors,
                            missing = Field#field.missing andalso not HasValue}, AllData);
apply_rules(#field{rules = Rule} = Field, AllData) when is_binary(Rule) ->
    {NewInp, Output, Errors, HasValue} = apply_one_rule(Field, {Rule, []}, AllData),
    apply_rules(Field#field{input = NewInp, rules = [], output = Output, errors = Errors,
                            missing = Field#field.missing andalso not HasValue}, AllData);
apply_rules(#field{rules = [Rule | Rest]} = Field, AllData) ->
    case apply_one_rule(Field, Rule, AllData) of
        {NewInp, Output, [], HasValue} ->
            apply_rules(Field#field{rules = Rest, input = NewInp, output = Output, errors = [],
                                    missing = Field#field.missing andalso not HasValue}, AllData);
        {_, Error, Error, _} -> apply_rules(Field#field{rules = [], output = Error, errors = Error}, AllData)
    end.

apply_one_rule(Field, Rule, AllData) when is_binary(Rule) ->
    apply_one_rule(Field, Rule, [], AllData);
apply_one_rule(Field, [{Rule, Args}], AllData) ->
    apply_one_rule(Field, Rule, Args, AllData);
apply_one_rule(Field, {Rule, Args}, AllData) ->
    apply_one_rule(Field, Rule, Args, AllData).

apply_one_rule(Field, Rule, Args, AllData) ->
    case rule_to_atom(Rule) of
        undefined -> process_result(apply_user_rules(Field, Rule, Args, AllData), Field#field.input);
        {_Module, _Function} when Field#field.input =:= null, not ?SEES_NULL(Rule) ->
            process_result({ok, null}, null);
        {Module, Function} ->
            process_result(erlang:apply(Module, Function, [Field#field.input, Args, AllData]), Field#field.input)
    end.

apply_user_rules(Field, Rule, Args, AllData) ->
    case apply_aliased_rules(Field, Rule, AllData) of
        undefined -> apply_registered_rules(Field, Rule, Args);
        Res -> Res
    end.

apply_aliased_rules(Field, RuleName, AllData) ->
    case lookup_alias(RuleName) of
        undefined ->
            undefined;
        {RuleName, Rules, undefined} ->
            case apply_rules(Field#field{rules = Rules, errors = []}, AllData) of
                #field{errors = []} = NewField -> alias_result(Field, NewField);
                NewField -> {error, NewField#field.errors}
            end;
        {RuleName, Rules, Error} ->
            case apply_rules(Field#field{rules = Rules, errors = []}, AllData) of
                #field{errors = []} = NewField -> alias_result(Field, NewField);
                _ -> {error, Error}
            end
    end.

%% A missing field gets a value when a rule of the alias (like "default") gives it one
alias_result(#field{missing = true}, #field{missing = false, output = Output}) ->
    {filter, Output};
alias_result(_Field, NewField) ->
    {ok, NewField#field.output}.

apply_registered_rules(Field, RuleName, Args) ->
    case lookup_rules(RuleName) of
        undefined ->
            {undefined, RuleName};
        {RuleName, Module, Function} ->
            erlang:apply(Module, Function, [Field#field.input, Args])
    end.

%% {NewInput, Output, Errors, HasValue}: HasValue tells whether the rule gave the field a value
%% (a modifier always does, a validator only when it changes the value)
process_result({undefined, RuleName}, Input) ->
    {Input, Input, <<"Rule '", RuleName/binary, "' not_registered">>, false};
process_result({filter, Output}, _) ->
    {Output, Output, [], true};
process_result({ok, Output}, Input) ->
    {Input, Output, [], Output =/= Input};
process_result({error, Error}, Input) ->
    {Input, Error, Error, false};
process_result(Unexpected, Input) ->
    ct:print("Unexpected: ~p~nInput:~p~n", [Unexpected, Input]).

rule_to_atom(<<"required">>) -> {olifer_commons, required};
rule_to_atom(<<"not_empty">>) -> {olifer_commons, not_empty};
rule_to_atom(<<"not_empty_list">>) -> {olifer_commons, not_empty_list};
rule_to_atom(<<"any_object">>) -> {olifer_commons, any_object};

rule_to_atom(<<"integer">>) -> {olifer_numeric, integer};
rule_to_atom(<<"positive_integer">>) -> {olifer_numeric, positive_integer};
rule_to_atom(<<"decimal">>) -> {olifer_numeric, decimal};
rule_to_atom(<<"positive_decimal">>) -> {olifer_numeric, positive_decimal};
rule_to_atom(<<"max_number">>) -> {olifer_numeric, max_number};
rule_to_atom(<<"min_number">>) -> {olifer_numeric, min_number};
rule_to_atom(<<"number_between">>) -> {olifer_numeric, number_between};

rule_to_atom(<<"one_of">>) -> {olifer_string, one_of};
rule_to_atom(<<"max_length">>) -> {olifer_string, max_length};
rule_to_atom(<<"min_length">>) -> {olifer_string, min_length};
rule_to_atom(<<"length_between">>) -> {olifer_string, length_between};
rule_to_atom(<<"length_equal">>) -> {olifer_string, length_equal};
rule_to_atom(<<"like">>) -> {olifer_string, like};
rule_to_atom(<<"string">>) -> {olifer_string, string};
rule_to_atom(<<"eq">>) -> {olifer_string, eq};

rule_to_atom(<<"email">>) -> {olifer_special, email};
rule_to_atom(<<"url">>) -> {olifer_special, url};
rule_to_atom(<<"iso_date">>) -> {olifer_special, iso_date};
rule_to_atom(<<"equal_to_field">>) -> {olifer_special, equal_to_field};

rule_to_atom(<<"nested_object">>) -> {olifer_metarules, nested_object};
rule_to_atom(<<"list_of">>) -> {olifer_metarules, list_of};
rule_to_atom(<<"list_of_objects">>) -> {olifer_metarules, list_of_objects};
rule_to_atom(<<"list_of_different_objects">>) -> {olifer_metarules, list_of_different_objects};
rule_to_atom(<<"variable_object">>) -> {olifer_metarules, variable_object};
rule_to_atom(<<"or">>) -> {olifer_metarules, 'or'};

rule_to_atom(<<"trim">>) -> {olifer_modifiers, trim};
rule_to_atom(<<"to_lc">>) -> {olifer_modifiers, to_lc};
rule_to_atom(<<"to_uc">>) -> {olifer_modifiers, to_uc};
rule_to_atom(<<"remove">>) -> {olifer_modifiers, remove};
rule_to_atom(<<"leave_only">>) -> {olifer_modifiers, leave_only};
rule_to_atom(<<"default">>) -> {olifer_modifiers, default};

rule_to_atom(Name) -> livr_2_5_rule(Name).

%% A rule that the user registers with the name of a LIVR 2.5 rule replaces it
livr_2_5_rule(Name) ->
    case livr_2_5_rule_to_atom(Name) of
        undefined ->
            undefined;
        Rule ->
            case lookup_alias(Name) =:= undefined andalso lookup_rules(Name) =:= undefined of
                true -> Rule;
                false -> undefined
            end
    end.

livr_2_5_rule_to_atom(<<"boolean">>) -> {olifer_commons, boolean};
livr_2_5_rule_to_atom(<<"required_if">>) -> {olifer_commons, required_if};
livr_2_5_rule_to_atom(<<"any">>) -> {olifer_commons, any};

livr_2_5_rule_to_atom(<<"max_decimals">>) -> {olifer_numeric, max_decimals};

livr_2_5_rule_to_atom(<<"not_one_of">>) -> {olifer_string, not_one_of};
livr_2_5_rule_to_atom(<<"digits">>) -> {olifer_string, digits};
livr_2_5_rule_to_atom(<<"starts_with">>) -> {olifer_string, starts_with};
livr_2_5_rule_to_atom(<<"ends_with">>) -> {olifer_string, ends_with};
livr_2_5_rule_to_atom(<<"contains">>) -> {olifer_string, contains};

livr_2_5_rule_to_atom(<<"iso_datetime">>) -> {olifer_special, iso_datetime};
livr_2_5_rule_to_atom(<<"iso_time">>) -> {olifer_special, iso_time};

livr_2_5_rule_to_atom(<<"when">>) -> {olifer_metarules, 'when'};
livr_2_5_rule_to_atom(<<"values">>) -> {olifer_metarules, values};
livr_2_5_rule_to_atom(<<"keys">>) -> {olifer_metarules, keys};

livr_2_5_rule_to_atom(<<"list_length">>) -> {olifer_lists, list_length};
livr_2_5_rule_to_atom(<<"unique">>) -> {olifer_lists, unique};

livr_2_5_rule_to_atom(<<"split">>) -> {olifer_modifiers, split};

livr_2_5_rule_to_atom(<<"uuid">>) -> {olifer_formats, uuid};
livr_2_5_rule_to_atom(<<"ip">>) -> {olifer_formats, ip};
livr_2_5_rule_to_atom(<<"cidr">>) -> {olifer_formats, cidr};
livr_2_5_rule_to_atom(<<"hostname">>) -> {olifer_formats, hostname};
livr_2_5_rule_to_atom(<<"base64">>) -> {olifer_formats, base64};
livr_2_5_rule_to_atom(<<"hex">>) -> {olifer_formats, hex};
livr_2_5_rule_to_atom(<<"digest">>) -> {olifer_formats, digest};
livr_2_5_rule_to_atom(<<"mongo_id">>) -> {olifer_formats, mongo_id};
livr_2_5_rule_to_atom(<<"hex_color">>) -> {olifer_formats, hex_color};
livr_2_5_rule_to_atom(<<"mac">>) -> {olifer_formats, mac};
livr_2_5_rule_to_atom(<<"e164">>) -> {olifer_formats, e164};
livr_2_5_rule_to_atom(<<"semver">>) -> {olifer_formats, semver};
livr_2_5_rule_to_atom(<<"jwt">>) -> {olifer_formats, jwt};

livr_2_5_rule_to_atom(<<"luhn">>) -> {olifer_checksums, luhn};
livr_2_5_rule_to_atom(<<"credit_card">>) -> {olifer_checksums, credit_card};
livr_2_5_rule_to_atom(<<"iban">>) -> {olifer_checksums, iban};

livr_2_5_rule_to_atom(_) -> undefined.

%% The rules of a missing field in their order: the rules that can fail or give it a value
%% (?SPEC_RULES) and aliases. Other rules skip a missing value.
has_spec_rule([FieldRules]) when is_list(FieldRules) ->
    has_spec_rule(FieldRules);
has_spec_rule(FieldRules) ->
    case [Rule || Rule <- rules_list(FieldRules), is_spec_rule(rule_name(Rule))] of
        [] -> false;
        SpecRules -> SpecRules
    end.

%% A list of rules or a rule with arguments ([{Name, Args}]), or a rule name
rules_list(FieldRules) when is_list(FieldRules) ->
    FieldRules;
rules_list(FieldRule) ->
    [FieldRule].

rule_name({Name, _Args}) ->
    Name;
rule_name([{Name, _Args}]) ->
    Name;
rule_name(Name) ->
    Name.

is_spec_rule(Name) when is_binary(Name) ->
    lists:member(Name, ?SPEC_RULES)
        orelse (rule_to_atom(Name) =:= undefined andalso lookup_alias(Name) =/= undefined);
is_spec_rule(_Name) ->
    false.

start(AppName) ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    ok = load(AppName),
    {ok, Dependencies} = application:get_key(AppName, applications),
    [begin
         ok = start(A)
     end || A <- Dependencies, not lists:member(A, RunningApps)],
    ok = application:start(AppName).

load(AppName) ->
    F = fun({App, _, _}) -> App end,
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(AppName, LoadedApps) of
        true ->
            ok;
        false ->
            ok = application:load(AppName)
    end.