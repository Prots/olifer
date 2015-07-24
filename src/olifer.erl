-module(olifer).

-include("olifer.hrl").

-export([validate/2]).
-export([register_rule/1]).
-export([start/0, stop/0]).
-export([rule_to_atom/1]).
-export([decode/1]).
-export([apply_rules/1]).

%% API
validate(Rules, Data) ->
    RulesPropList = decode(Rules),
    DataPropList = decode(Data),
    ListOfFields = prevalidate(DataPropList, RulesPropList),
    lists:reverse([apply_rules(Field) || Field <- ListOfFields]).

register_rule(Data) ->
    ets:insert(?RULES_TBL, Data).

start() ->
    start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% INTERNAL

%% 4. Exclude all fields that do not have validation rules described
prevalidate(DataPropList, RulesPropList) ->
    prevalidate(DataPropList, RulesPropList, []).

prevalidate(_DataPropList, [], Acc) ->
    Acc;
prevalidate([], [{FieldName, FieldRules}|RestRules], Acc) ->
    case has_required_rule(FieldRules) of
        true -> prevalidate([], RestRules, [apply_rules(#field{name = FieldName, rules = FieldRules})|Acc]);
        false -> prevalidate([], RestRules, Acc)
    end;
prevalidate([{FieldName, FieldData}|RestData], RulesPropList, Acc) ->
    {NewAcc, RestRules} = case lists:keyfind(FieldName, 1, RulesPropList) of
        false ->
            {Acc, RulesPropList};
        {FieldName, FieldRules} ->
            {[apply_rules(#field{name = FieldName, input = FieldData, rules = FieldRules})|Acc],
                proplists:delete(FieldName, RulesPropList)}
    end,
    prevalidate(RestData, RestRules, NewAcc).

apply_rules(#field{rules = []} = Field) ->
    Field;
apply_rules(#field{input = Input, rules = {Rule, Args}} = Field) ->
    {Output, Errors} = apply_one_rule(Input, {Rule, Args}),
    apply_rules(Field#field{rules = [], output = Output, errors = Errors});
apply_rules(#field{input = Input, rules = Rule} = Field) when is_binary(Rule) ->
    {Output, Errors} = apply_one_rule(Input, {Rule, []}),
    apply_rules(Field#field{rules = [], output = Output, errors = Errors});
apply_rules(#field{input = Input, rules = [Rule|Rest]} = Field) ->
    {Output, Errors} = apply_one_rule(Input, Rule),
    apply_rules(Field#field{rules = Rest, output = Output, errors = Errors}).

apply_one_rule(Input, Rule) when is_binary(Rule) ->
    process_result(erlang:apply(olifer_rules, rule_to_atom(Rule), [Input, []]));
apply_one_rule(Input, [{Rule, Args}]) ->
    process_result(erlang:apply(olifer_rules, rule_to_atom(Rule), [Input, Args]));
apply_one_rule(Input, {Rule, Arg}) ->
    process_result(erlang:apply(olifer_rules, rule_to_atom(Rule), [Input, Arg])).

process_result({ok, Input}) -> {Input, []};
process_result({error, Error}) -> {Error, Error}.

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
rule_to_atom(_) ->                                  undefined.

%% TODO this is fucking hack, but without it 'required' rule doesn't work!!!
has_required_rule([]) ->
    false;
has_required_rule([<<"required">>|_Rest]) ->
    true;
has_required_rule([{<<"required">>, _}|_Rest]) ->
    true;
has_required_rule([[{<<"required">>, _}]|_Rest]) ->
    true;
has_required_rule([_|Rest]) ->
    has_required_rule(Rest).

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