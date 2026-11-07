-module(olifer_schema).

-include("olifer.hrl").

%% The schema check of LIVR 2.5: an unknown rule, arguments that do not match a rule, or an empty
%% list of rules where a LIVR 2.5 rule takes rules fail with error({schema_error, Message}).
%% The LIVR 2.0 rules ignore their arguments as before (except iso_date, which takes options now),
%% but the rules inside them are checked too.

%% API
-export([check/1]).
%% Arguments, for the rules
-export([non_negative_integer/1, string_arg/1, rules_arg/1]).

%% API
check(Rules) ->
    check_fields(Rules, []).

%% A non-negative integer, also as a number with a zero fraction or as a string: 2, 2.0, "2"
non_negative_integer(Value) when is_integer(Value), Value >= 0 ->
    Value;
non_negative_integer(Value) when is_float(Value), Value >= 0, Value == trunc(Value) ->
    trunc(Value);
non_negative_integer(Value) when is_binary(Value) ->
    case olifer_numeric:decimal_number(Value) of
        error -> error;
        Number -> non_negative_integer(Number)
    end;
non_negative_integer(_Value) ->
    error.

%% A string, or a number as its string form
string_arg(Value) when is_binary(Value); is_number(Value) ->
    olifer:to_binary(Value);
string_arg(_Value) ->
    error.

%% The rules of "values" and "keys", as in "list_of": "rule", ["rule1", "rule2"] or [["rule1", "rule2"]]
rules_arg([Rules | _] = Args) when is_list(Rules) ->
    case olifer:is_object(Rules) of
        true -> Args;
        false -> Rules
    end;
rules_arg(Rules) ->
    Rules.

%% INTERNAL
%% Context: the aliases on the way, so that a recursive alias is checked once
check_fields(Fields, Context) when is_list(Fields) ->
    lists:foreach(fun({_Name, Rules}) -> check_rules(Rules, Context);
                     (_) -> ok
                  end, Fields);
check_fields(_Fields, _Context) ->
    ok.

%% A rule name, a rule object (every key is a rule) or a list of rules
check_rules(Name, Context) when is_binary(Name) ->
    check_rule(Name, [], Context);
check_rules({Name, Args}, Context) ->
    check_rule(Name, Args, Context);
check_rules(Rules, Context) when is_list(Rules) ->
    lists:foreach(fun(Rule) -> check_rules(Rule, Context) end, Rules);
check_rules(_Rules, _Context) ->
    ok.

check_rule(Name, Args, Aliases) ->
    case olifer:rule_kind(Name) of
        builtin ->
            check_args(Name, rule_args(Name, Args), Aliases);
        {alias, Rules} ->
            case lists:member(Name, Aliases) of
                true -> ok;
                false -> check_rules(Rules, [Name | Aliases])
            end;
        user ->
            ok;
        undefined ->
            schema_error(Name, <<"is not registered">>)
    end.

%% A single null is one argument, a schema error for the LIVR 2.5 rules. iso_date ignored its
%% arguments in LIVR 2.0, so {"iso_date": null} stays the plain iso_date
rule_args(<<"iso_date">>, Args) ->
    olifer_special:iso_date_args(Args);
rule_args(_Name, Args) ->
    olifer:args(Args).

%% Rules with rules inside
check_args(<<"nested_object">>, [Fields | _], Context) ->
    check_fields(Fields, Context);
check_args(<<"list_of_objects">>, [Fields | _], Context) ->
    check_fields(Fields, Context);
check_args(<<"variable_object">>, [_Field, Types | _], Context) ->
    check_types(Types, Context);
check_args(<<"list_of_different_objects">>, [_Field, Types | _], Context) ->
    check_types(Types, Context);
check_args(<<"list_of">>, Args, Context) ->
    check_rules(rules_arg(Args), Context);
check_args(<<"or">>, RuleSets, Context) ->
    check_rules(RuleSets, Context);
check_args(<<"when">> = Rule, [Reference, Cases], Context) ->
    check_reference(Rule, Reference),
    case olifer:is_object(Cases) andalso Cases =/= [{}] of
        true -> lists:foreach(fun({_Case, Rules}) -> check_rules_arg(Rule, Rules, Context) end, Cases);
        false -> schema_error(Rule, <<"takes an object of cases">>)
    end;
check_args(<<"when">> = Rule, [Reference, Condition, RulesIfTrue | RulesIfFalse], Context) ->
    check_reference(Rule, Reference),
    lists:foreach(fun(Rules) -> check_rules_arg(Rule, Rules, Context) end,
                  [Condition, RulesIfTrue | lists:sublist(RulesIfFalse, 1)]);
check_args(<<"when">> = Rule, _Args, _Context) ->
    schema_error(Rule, <<"takes a reference and cases, or a reference, a condition and rules">>);
check_args(<<"values">> = Rule, Args, Context) ->
    check_rules_arg(Rule, rules_arg(Args), Context);
check_args(<<"keys">> = Rule, Args, Context) ->
    check_rules_arg(Rule, rules_arg(Args), Context);

%% Rules without arguments
check_args(Rule, Args, _Context) when Rule =:= <<"boolean">>; Rule =:= <<"any">>;
                                   Rule =:= <<"digits">>; Rule =:= <<"hostname">>;
                                   Rule =:= <<"hex">>; Rule =:= <<"mongo_id">>;
                                   Rule =:= <<"hex_color">>; Rule =:= <<"e164">>;
                                   Rule =:= <<"semver">>; Rule =:= <<"jwt">>;
                                   Rule =:= <<"luhn">>; Rule =:= <<"credit_card">>;
                                   Rule =:= <<"iban">> ->
    case Args of
        [] -> ok;
        _ -> schema_error(Rule, <<"takes no arguments">>)
    end;

%% Rules with arguments
check_args(<<"not_one_of">> = Rule, Values, _Context) ->
    case Values =/= [] andalso lists:all(fun olifer:is_primitive/1, Values) of
        true -> ok;
        false -> schema_error(Rule, <<"takes one or more strings, numbers or booleans">>)
    end;
check_args(<<"max_decimals">> = Rule, Args, _Context) ->
    case Args of
        [Decimals | _] -> non_negative_integer_arg(Rule, Decimals);
        _ -> schema_error(Rule, <<"takes a non-negative integer">>)
    end;
check_args(Rule, Strings, _Context) when Rule =:= <<"starts_with">>; Rule =:= <<"ends_with">>;
                                      Rule =:= <<"contains">> ->
    IsString = fun(String) -> not lists:member(string_arg(String), [error, <<>>]) end,
    case Strings =/= [] andalso lists:all(IsString, Strings) of
        true -> ok;
        false -> schema_error(Rule, <<"takes one or more non-empty strings">>)
    end;
check_args(<<"iso_date">> = Rule, Args, _Context) ->
    case bounds(Rule, Args, fun(Bound, _Option) -> olifer_special:date_bound(Bound) end) of
        {{date, Min}, {date, Max}} when Min > Max -> min_after_max(Rule);
        {{days, Min}, {days, Max}} when Min > Max -> min_after_max(Rule);
        _ -> ok
    end;
check_args(<<"iso_datetime">> = Rule, Args, _Context) ->
    case bounds(Rule, Args, fun olifer_special:datetime_bound/2) of
        {{instant, Min}, {instant, Max}} when Min > Max -> min_after_max(Rule);
        {{days, Min}, {days, Max}} when Min > Max -> min_after_max(Rule);
        _ -> ok
    end;
check_args(<<"iso_time">> = Rule, Args, _Context) ->
    _ = bounds(Rule, Args, fun(Bound, _Option) -> olifer_special:parse_time(Bound) end),
    ok;
check_args(<<"required_if">> = Rule, [Conditions | _], _Context) ->
    case olifer:is_object(Conditions) andalso Conditions =/= [{}] of
        true -> lists:foreach(fun(Condition) -> check_condition(Rule, Condition) end, Conditions);
        false -> schema_error(Rule, <<"takes an object of references to values">>)
    end;
check_args(<<"required_if">> = Rule, [], _Context) ->
    schema_error(Rule, <<"takes an object of references to values">>);
check_args(<<"list_length">> = Rule, Args, _Context) ->
    case olifer_lists:length_bounds(Args) of
        {ok, Min, Max} when Max =:= infinity; Min =< Max -> ok;
        {ok, _Min, _Max} -> min_after_max(Rule);
        error -> schema_error(Rule, <<"takes a length, min and max, or an object with min and max">>)
    end;
check_args(<<"unique">> = Rule, Keys, _Context) ->
    case lists:member(error, [string_arg(Key) || Key <- Keys]) of
        false -> ok;
        true -> schema_error(Rule, <<"takes key names">>)
    end;
check_args(<<"split">> = Rule, Args, _Context) ->
    case Args of
        [Separator | _] when Separator =/= <<>> ->
            case string_arg(Separator) of
                error -> schema_error(Rule, <<"takes a non-empty separator string">>);
                _ -> ok
            end;
        _ ->
            schema_error(Rule, <<"takes a non-empty separator string">>)
    end;
check_args(<<"uuid">> = Rule, Versions, _Context) ->
    case lists:all(fun(Version) -> olifer_formats:uuid_version(Version) =/= error end, Versions) of
        true -> ok;
        false -> schema_error(Rule, <<"takes versions \"v1\" to \"v8\"">>)
    end;
check_args(Rule, Args, _Context) when Rule =:= <<"ip">>; Rule =:= <<"cidr">> ->
    case Args of
        [] -> ok;
        [Version | _] when Version =:= <<"v4">>; Version =:= <<"v6">> -> ok;
        _ -> schema_error(Rule, <<"takes no arguments, \"v4\" or \"v6\"">>)
    end;
check_args(<<"base64">> = Rule, Args, _Context) ->
    case Args of
        [] -> ok;
        [<<"url">> | _] -> ok;
        _ -> schema_error(Rule, <<"takes no arguments or \"url\"">>)
    end;
check_args(<<"digest">> = Rule, Algorithms, _Context) ->
    IsAlgorithm = fun(Algorithm) -> olifer_formats:digest_length(Algorithm) =/= error end,
    case Algorithms =/= [] andalso lists:all(IsAlgorithm, Algorithms) of
        true -> ok;
        false -> schema_error(Rule, <<"takes one or more of md5, sha1, sha256, sha384, sha512">>)
    end;
check_args(<<"mac">> = Rule, Args, _Context) ->
    case Args of
        [] -> ok;
        [Bits | _] ->
            case non_negative_integer(Bits) of
                48 -> ok;
                64 -> ok;
                _ -> schema_error(Rule, <<"takes no arguments, 48 or 64">>)
            end
    end;

%% LIVR 2.0 rules
check_args(_Rule, _Args, _Context) ->
    ok.

%% The objects of "variable_object" and "list_of_different_objects" by their types
check_types(Types, Context) when is_list(Types) ->
    lists:foreach(fun({_Type, Fields}) -> check_fields(Fields, Context); (_) -> ok end, Types);
check_types(_Types, _Context) ->
    ok.

%% The rules of "when", "values" and "keys": a rule name, a rule object or a list of them, but
%% not an empty list, null or an empty list in the list
check_rules_arg(Rule, [], _Context) ->
    schema_error(Rule, <<"got an empty list of rules, use \"any\" for no rules">>);
check_rules_arg(Rule, Rules, Context) when is_binary(Rules); is_list(Rules) ->
    IsRule = fun(R) -> is_binary(R) orelse olifer:is_object(R) end,
    case is_binary(Rules) orelse olifer:is_object(Rules) orelse lists:all(IsRule, Rules) of
        true -> check_rules(Rules, Context);
        false -> schema_error(Rule, <<"takes rules">>)
    end;
check_rules_arg(Rule, _Rules, _Context) ->
    schema_error(Rule, <<"takes rules">>).

check_reference(Rule, Reference) ->
    case olifer:parse_reference(Reference) of
        {ok, _Path} -> ok;
        error -> schema_error(Rule, <<"got a wrong reference">>)
    end.

%% A reference and a value other than "" and null, or a non-empty list of such values
check_condition(Rule, {Reference, Values}) ->
    check_reference(Rule, Reference),
    IsValue = fun(Value) -> olifer:is_primitive(Value) andalso Value =/= <<>> end,
    IsCondition = case is_list(Values) andalso not olifer:is_object(Values) of
        true -> Values =/= [] andalso lists:all(IsValue, Values);
        false -> IsValue(Values)
    end,
    case IsCondition of
        true -> ok;
        false -> schema_error(Rule, <<"takes a value other than \"\" or a non-empty list of them">>)
    end.

non_negative_integer_arg(Rule, Value) ->
    case non_negative_integer(Value) of
        error -> schema_error(Rule, <<"takes a non-negative integer">>);
        _ -> ok
    end.

%% No arguments, or an object with "min" and "max" first: the parsed bounds
bounds(_Rule, [], _Parse) ->
    {undefined, undefined};
bounds(Rule, [Options | _], Parse) ->
    case olifer:is_object(Options) of
        true -> ok;
        false -> schema_error(Rule, <<"takes no arguments or an object with min and max">>)
    end,
    case [Key || {Key, _} <- Options, Key =/= <<"min">>, Key =/= <<"max">>] of
        [] -> ok;
        [Key | _] -> schema_error(Rule, <<"got an unknown option \"", Key/binary, "\"">>)
    end,
    {bound(Rule, Options, min, Parse), bound(Rule, Options, max, Parse)}.

bound(Rule, Options, Option, Parse) ->
    case olifer:get_value(atom_to_binary(Option, utf8), Options) of
        undefined ->
            undefined;
        Bound ->
            case Parse(Bound, Option) of
                error -> schema_error(Rule, <<"got a wrong bound">>);
                Parsed -> Parsed
            end
    end.

min_after_max(Rule) ->
    schema_error(Rule, <<"got min greater than max">>).

schema_error(Rule, Message) when is_binary(Rule) ->
    erlang:error({schema_error, <<"Rule '", Rule/binary, "' ", Message/binary>>});
schema_error(Rule, Message) ->
    schema_error(iolist_to_binary(io_lib:format("~0p", [Rule])), Message).
