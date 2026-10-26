-module(olifer).

-include("olifer.hrl").

%% LIVR API
-export([validate/2]).
-export([register_rule/3]).
-export([register_aliased_rule/1]).
%% FOR INTERNAL USAGE
-export([start/0, stop/0]).
-export([validate_data/2]).
-export([validate_value/2]).
-export([get_value/2]).
-export([to_binary/1]).

%% LIVR API
validate([], []) ->
    {ok, [{}]};
validate([{}], []) ->
    {ok, [{}]};
validate(_Data, [{}]) ->
    {ok, [{}]};
validate(Data, Rules) when is_list(Data), is_list(Rules) ->
    FieldsList = validate_data(Data, Rules),
    {Type, Result} = return_result(FieldsList, [], []),
    {Type, lists:reverse(Result)};
validate(Data, _Rules) ->
    [{Data, ?FORMAT_ERROR}].

register_rule(Name, Module, Function) when is_atom(Module), is_atom(Function) ->
    true = ets:insert(?RULES_TBL, {Name, Module, Function}),
    ok.

register_aliased_rule(Aliases) when is_list(Aliases) ->
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
    Field = #field{name = value, input = Value, rules = Rules, output = Value, errors = [], missing = true},
    case apply_rules(Field, [{value, Value}]) of
        #field{errors = [], output = Output, missing = Missing} -> {ok, Output, not Missing};
        #field{errors = Errors} -> {error, Errors}
    end.

%% INTERNAL
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

rule_to_atom(_) -> undefined.

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