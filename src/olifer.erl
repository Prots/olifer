-module(olifer).

-include("olifer.hrl").

-export([validate/2]).
-export([register_rule/1]).
-export([rule_to_atom/1]).
-export([decode/1]).
-export([apply_rules/2]).
-export([prevalidate/2]).

%% API
validate(Data, Rules) when is_binary(Data), is_binary(Rules) ->
    RulesPropList = decode(Rules),
    DataPropList = decode(Data),
    validate(DataPropList, RulesPropList);
validate(DataPropList, RulesPropList) when is_list(DataPropList), is_list(RulesPropList) ->
    ListOfFields = prevalidate(DataPropList, RulesPropList),
    lists:reverse([apply_rules(Field, DataPropList) || Field <- ListOfFields]);
validate(Data, Rules) ->
    [#field{name = Data, input = Data, rules = Rules, output = ?FORMAT_ERROR, errors = ?FORMAT_ERROR}].

register_rule(Data) ->
    ets:insert(?RULES_TBL, Data).

%% INTERNAL

prevalidate(DataPropList, RulesPropList) ->
    prevalidate(DataPropList, DataPropList, RulesPropList, []).

prevalidate(_DataPropList, _AllData, [], Acc) ->
%%     ct:print("Acc: ~p~n", [Acc]),
    Acc;
prevalidate([], AllData, [{FieldName, FieldRules}|RestRules], Acc) ->
    case has_spec_rule(FieldRules) of
        true -> prevalidate([], AllData, RestRules, [#field{name = FieldName, input= <<>>, rules = FieldRules}|Acc]);
        false -> prevalidate([], AllData, RestRules, Acc)
    end;
prevalidate([{FieldName, FieldData}|RestData], AllData, RulesPropList, Acc) ->
    {NewAcc, RestRules} = case lists:keyfind(FieldName, 1, RulesPropList) of
        false ->
            {Acc, RulesPropList};
        {FieldName, FieldRules} ->
            {[#field{name = FieldName, input = FieldData, rules = FieldRules}|Acc], proplists:delete(FieldName, RulesPropList)}
    end,
    prevalidate(RestData, AllData, RestRules, NewAcc).

apply_rules(#field{rules = []} = Field, _AllData) ->
%%     ct:print("Field Result: ~p~n", [Field]),
    Field;
apply_rules(#field{input = Input, rules = {Rule, Args}} = Field, AllData) ->
    {NewInp, Output, Errors} = apply_one_rule(Input, {Rule, Args}, AllData),
    apply_rules(Field#field{input = NewInp, rules = [], output = Output, errors = Errors}, AllData);
apply_rules(#field{input = Input, rules = Rule} = Field, AllData) when is_binary(Rule) ->
    {NewInp, Output, Errors} = apply_one_rule(Input, {Rule, []}, AllData),
    apply_rules(Field#field{input = NewInp, rules = [], output = Output, errors = Errors}, AllData);
apply_rules(#field{input = Input, rules = [Rule|Rest]} = Field, AllData) ->
%%     ct:print("Field intermediate: ~p~n", [Field]),
    case apply_one_rule(Input, Rule, AllData) of
        {NewInp, Output, []} -> apply_rules(Field#field{rules = Rest, input = NewInp, output = Output, errors = []}, AllData);
        {_, Error, Error} -> apply_rules(Field#field{rules = [], output = Error, errors = Error}, AllData)
    end.

apply_one_rule(Input, Rule, AllData) when is_binary(Rule) ->
    process_result(erlang:apply(olifer_rules, rule_to_atom(Rule), [Input, [], AllData]), Input);
apply_one_rule(Input, [{Rule, Args}], AllData) ->
    process_result(erlang:apply(olifer_rules, rule_to_atom(Rule), [Input, Args, AllData]), Input);
apply_one_rule(Input, {Rule, Arg}, AllData) ->
    process_result(erlang:apply(olifer_rules, rule_to_atom(Rule), [Input, Arg, AllData]), Input).

process_result({filter, Output}, _) -> {Output, Output, []};
process_result({ok, Output}, Input) -> {Input, Output, []};
process_result({error, Error}, Input) -> {Input, Error, Error}.

rule_to_atom(<<"required">>) ->                     required;
rule_to_atom(<<"not_empty">>) ->                    not_empty;
rule_to_atom(<<"not_empty_list">>) ->               not_empty_list;
rule_to_atom(<<"integer">>) ->                      integer;
rule_to_atom(<<"positive_integer">>) ->             positive_integer;
rule_to_atom(<<"decimal">>) ->                      decimal;
rule_to_atom(<<"positive_decimal">>) ->             positive_decimal;
rule_to_atom(<<"max_number">>) ->                   max_number;
rule_to_atom(<<"min_number">>) ->                   min_number;
rule_to_atom(<<"number_between">>) ->               number_between;
rule_to_atom(<<"one_of">>) ->                       one_of;
rule_to_atom(<<"max_length">>) ->                   max_length;
rule_to_atom(<<"min_length">>) ->                   min_length;
rule_to_atom(<<"length_between">>) ->               length_between;
rule_to_atom(<<"length_equal">>) ->                 length_equal;
rule_to_atom(<<"like">>) ->                         like;
rule_to_atom(<<"email">>) ->                        email;
rule_to_atom(<<"url">>) ->                          url;
rule_to_atom(<<"iso_date">>) ->                     iso_date;
rule_to_atom(<<"equal_to_field">>) ->               equal_to_field;
rule_to_atom(<<"nested_object">>) ->                nested_object;
rule_to_atom(<<"list_of">>) ->                      list_of;
rule_to_atom(<<"list_of_objects">>) ->              list_of_objects;
rule_to_atom(<<"list_of_different_objects">>) ->    list_of_different_objects;
rule_to_atom(<<"trim">>) ->                         trim;
rule_to_atom(<<"to_lc">>) ->                        to_lc;
rule_to_atom(<<"to_uc">>) ->                        to_uc;
rule_to_atom(<<"remove">>) ->                       remove;
rule_to_atom(<<"leave_only">>) ->                   leave_only;
rule_to_atom(_) ->                                  undefined.

%% TODO this is fucking hack, but without it 'required' rule doesn't work!!!

has_spec_rule(FieldRules) ->
    has_spec_rule(FieldRules, ?SPEC_RULES).

has_spec_rule(_FieldRules, []) ->
    false;
has_spec_rule(FieldRules, [Type|RestTypes]) ->
    case has_rule(FieldRules, Type) of
        true -> true;
        _ -> has_spec_rule(FieldRules, RestTypes)
    end.

has_rule(Type, Type) ->
    true;
has_rule([], _Type) ->
    false;
has_rule([Type|_Rest], Type) ->
    true;
has_rule([{Type, _}|_Rest], Type) ->
    true;
has_rule([[{Type, _}]|_Rest], Type) ->
    true;
has_rule([_|Rest], Type) ->
    has_rule(Rest, Type);
has_rule(_, _) ->
    false.

decode(BinaryData) ->
    try
        jsx:decode(BinaryData)
    catch
        _:_ -> json_parsing_error
    end.
