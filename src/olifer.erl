-module(olifer).

-include("olifer.hrl").

-export([start/0, stop/0]).
-export([validate/2]).
-export([register_rule/3]).
-export([register_aliased_rule/1]).
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

register_rule(Name, Module, Function) when is_atom(Module), is_atom(Function) ->
    true = ets:insert(?RULES_TBL, {Name, Module, Function}),
    ok.

register_aliased_rule(AliasesJson) ->
    Aliases = decode(AliasesJson),
    register_aliases(Aliases).

start() ->
    start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% INTERNAL
register_aliases([]) ->
    ok;
register_aliases([Alias|Rest]) ->
    Name = proplists:get_value(<<"name">>, Alias),
    Rules = proplists:get_value(<<"rules">>, Alias),
    ErrorCode = proplists:get_value(<<"error">>, Alias),
    true = ets:insert(?ALIASES_TBL, {Name, Rules, ErrorCode}),
    register_aliases(Rest).

lookup_alias(Name) ->
    case ets:lookup(?ALIASES_TBL, Name) of
        [] -> undefined;
        [{Name, Rules, Error}|_] -> {Name, Rules, Error}
    end.

lookup_rules(Name) ->
    case ets:lookup(?RULES_TBL, Name) of
        [] -> undefined;
        [{Name, Module, Function}|_] -> {Name, Module, Function}
    end.

prevalidate(DataPropList, RulesPropList) ->
    prevalidate(DataPropList, DataPropList, RulesPropList, []).

prevalidate(_DataPropList, _AllData, [], Acc) ->
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
    Field;
apply_rules(#field{rules = {Rule, Args}} = Field, AllData) ->
    {NewInp, Output, Errors} = apply_one_rule(Field, {Rule, Args}, AllData),
    apply_rules(Field#field{input = NewInp, rules = [], output = Output, errors = Errors}, AllData);
apply_rules(#field{rules = Rule} = Field, AllData) when is_binary(Rule) ->
    {NewInp, Output, Errors} = apply_one_rule(Field, {Rule, []}, AllData),
    apply_rules(Field#field{input = NewInp, rules = [], output = Output, errors = Errors}, AllData);
apply_rules(#field{rules = [Rule|Rest]} = Field, AllData) ->
    case apply_one_rule(Field, Rule, AllData) of
        {NewInp, Output, []} -> apply_rules(Field#field{rules = Rest, input = NewInp, output = Output, errors = []}, AllData);
        {_, Error, Error} -> apply_rules(Field#field{rules = [], output = Error, errors = Error}, AllData)
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
        RuleAtom ->  process_result(erlang:apply(olifer_rules, RuleAtom, [Field#field.input, Args, AllData]), Field#field.input)
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
                #field{errors = []} = NewField -> {ok, NewField#field.output};
                NewField -> {error, NewField#field.errors}
            end;
        {RuleName, Rules, Error} ->
            case apply_rules(Field#field{rules = Rules, errors = []}, AllData) of
                #field{errors = []} = NewField -> {ok, NewField#field.output};
                _ -> {error, Error}
            end
    end.

apply_registered_rules(Field, RuleName, Args) ->
    case lookup_rules(RuleName) of
        undefined ->
            undefined;
        {RuleName, Module, Function} ->
            erlang:apply(Module, Function, [Field#field.input, Args])
    end.

process_result(undefined, Input) -> {Input, Input, []};
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

%% TODO this is fucking hack, but without it 'required' and 'not_empty_list' rules doesn't work!!!

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