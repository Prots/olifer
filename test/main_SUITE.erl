-module(main_SUITE).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-include("olifer.hrl").

-define(LIVR_TEST_PATH, "deps/LIVR/test_suite/").
-define(LIVR_2_5_TEST_PATH, "deps/LIVR/test_suite_2.5/").
-define(CT_TEST_PATH, "test/register_suite/").

-define(TEST_EXCEPTION_LIST, []).

-export([all/0]).
-export([test_positive/1, test_negative/1]).
-export([test_aliases_positive/1, test_aliases_negative/1]).
-export([test_register_positive/1, test_register_negative/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([test_border_positive/1, test_border_negative/1, test_border_schema_errors/1]).
-export([test_2_5_positive/1, test_2_5_negative/1]).
-export([test_2_5_aliases_positive/1, test_2_5_aliases_negative/1]).
-export([test_2_5_schema_errors/1]).
-export([test_null_arguments/1, test_unknown_rules/1]).

all() ->
    [
        test_positive,
        test_negative,
        test_aliases_positive,
        test_aliases_negative,
        test_register_positive,
        test_register_negative,
        test_border_positive,
        test_border_negative,
        test_border_schema_errors,
        test_null_arguments,
        test_unknown_rules,
        test_2_5_positive,
        test_2_5_negative,
        test_2_5_aliases_positive,
        test_2_5_aliases_negative,
        %% the last one: the aliases of these cases have errors
        test_2_5_schema_errors
    ].

init_per_suite(Config) ->
    ok = olifer:start(),
    Config.

end_per_suite(Config) ->
    ok = olifer:stop(),
    Config.

%% TESTS
test_positive(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         case lists:member(TestDir, ?TEST_EXCEPTION_LIST) of
             true ->
                 ok;
             _ ->
                 {Rules, Input, Output} = get_working_data(positive, TestDir, WholePath),
                 Result = olifer:validate(Input, Rules),
                 ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~nResult: ~p~n",
                     [TestDir, Rules, Input, Output, Result]),
                 ?assertEqual(unordered({ok, Output}), unordered(Result))
         end
     end || TestDir <- lists:sort(ListDir)].

test_negative(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         case lists:member(TestDir, ?TEST_EXCEPTION_LIST) of
             true ->
                 ok;
             _ ->
                 {Rules, Input, Errors} = get_working_data(negative, TestDir, WholePath),
                 Result = {_, PartRes} = olifer:validate(Input, Rules),
                 ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nErrors: ~p,~nResult: ~p,~n",
                     [TestDir, Rules, Input, Errors, PartRes]),
                 ?assertEqual(unordered({errors, Errors}), unordered(Result))
         end
     end || TestDir <- lists:sort(ListDir)].

test_aliases_positive(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "aliases_positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Output} = get_working_data(aliases_positive, TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p,~nRules: ~p,~nInput: ~p,~nOutput: ~p,~n",
             [TestDir, Aliases, Rules, Input, Output]),
         ok = olifer:register_aliased_rule(Aliases),
         ?assertEqual(unordered({ok, Output}), unordered(olifer:validate(Input, Rules)))
     end || TestDir <- lists:sort(ListDir)].

test_aliases_negative(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "aliases_negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Errors} = get_working_data(aliases_negative, TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p, Rules: ~p,~nInput: ~p,~nOutput: ~p,~n",
             [TestDir, Aliases, Rules, Input, Errors]),
         ok = olifer:register_aliased_rule(Aliases),
         ?assertEqual(unordered({errors, Errors}), unordered(olifer:validate(Input, Rules)))
     end || TestDir <- lists:sort(ListDir)].

test_register_positive(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?CT_TEST_PATH ++ "positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Output} = get_working_data(register_positive, TestDir, WholePath),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Rules, Input, Output]),
         ok = olifer:register_rule(list_to_binary(TestDir), new_rules, list_to_atom(TestDir)),
         ?assertEqual(unordered({ok, Output}), unordered(olifer:validate(Input, Rules)))
     end || TestDir <- lists:sort(ListDir)].

test_register_negative(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?CT_TEST_PATH ++ "negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Errors} = get_working_data(register_negative, TestDir, WholePath),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Rules, Input, Errors]),
         ok = olifer:register_rule(list_to_binary(TestDir), new_rules, list_to_atom(TestDir)),
         ?assertEqual(unordered({errors, Errors}), unordered(olifer:validate(Input, Rules)))
     end || TestDir <- lists:sort(ListDir)].

test_border_positive(_Config) ->
    WholePath = get_main_path() ++ "/test/border_suite/positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Output} = get_working_data(border_suite_positive, TestDir, WholePath),
         ok = register_border_aliases(WholePath ++ TestDir),
         {_, PartRes} = Result = olifer:validate(Input, Rules),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~nResult: ~p~n",
             [TestDir, Rules, Input, Output, PartRes]),
         ?assertEqual(unordered({ok, Output}), unordered(Result))
     end || TestDir <- lists:sort(ListDir)].

test_border_negative(_Config) ->
    WholePath = get_main_path() ++ "/test/border_suite/negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Errors} = get_working_data(border_suite_negative, TestDir, WholePath),
         ok = register_border_aliases(WholePath ++ TestDir),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nErrors: ~p,~n", [TestDir, Rules, Input, Errors]),
         ?assertEqual(unordered({errors, Errors}), unordered(olifer:validate(Input, Rules)))
     end || TestDir <- lists:sort(ListDir)].

test_border_schema_errors(_Config) ->
    test_schema_errors(get_main_path() ++ "/test/border_suite/schema_errors/").

%% LIVR 2.5
test_2_5_positive(_Config) ->
    test_2_5_suite("positive", ok, "output.json").

test_2_5_negative(_Config) ->
    test_2_5_suite("negative", errors, "errors.json").

test_2_5_aliases_positive(_Config) ->
    test_2_5_suite("aliases_positive", ok, "output.json").

test_2_5_aliases_negative(_Config) ->
    test_2_5_suite("aliases_negative", errors, "errors.json").

test_2_5_schema_errors(_Config) ->
    test_schema_errors(get_main_path() ++ "/" ++ ?LIVR_2_5_TEST_PATH ++ "schema_errors/").

%% {"rule": null} is one argument null: a schema error for the LIVR 2.5 rules. iso_date ignored its
%% arguments in LIVR 2.0, so {"iso_date": null} stays the plain iso_date. The other LIVR 2.0 rules
%% and the registered rules get null as one argument, as in LIVR 2.0
test_null_arguments(_Config) ->
    [begin
         ?assertEqual(ok, olifer:check_schema([{<<"x">>, [{<<"iso_date">>, Null}]}])),
         ?assertEqual({ok, [{<<"x">>, <<"2026-10-18">>}]},
                      olifer:validate([{<<"x">>, <<"2026-10-18">>}], [{<<"x">>, [{<<"iso_date">>, Null}]}])),
         ?assertEqual({errors, [{<<"x">>, <<"WRONG_DATE">>}]},
                      olifer:validate([{<<"x">>, <<"2026-02-30">>}], [{<<"x">>, [{<<"iso_date">>, Null}]}]))
     end || Null <- [null, [null]]],
    [begin
         ?assertMatch({error, _}, olifer:check_schema([{<<"x">>, [{Rule, Null}]}])),
         ?assertError({schema_error, _}, olifer:validate([{<<"x">>, <<"1">>}], [{<<"x">>, [{Rule, Null}]}]))
     end || Rule <- [<<"boolean">>, <<"any">>, <<"uuid">>, <<"ip">>, <<"base64">>, <<"mac">>, <<"unique">>,
                     <<"iso_datetime">>, <<"iso_time">>, <<"not_one_of">>, <<"split">>, <<"list_length">>,
                     <<"required_if">>, <<"max_decimals">>, <<"when">>, <<"values">>, <<"keys">>],
            Null <- [null, [null]]],
    ?assertMatch({error, _}, olifer:check_schema([{<<"x">>, [{<<"list_length">>, [{<<"min">>, null}]}]}])),
    %% null or an empty list in place of rules
    [?assertMatch({error, _}, olifer:check_schema([{<<"x">>, Rules}]))
     || Rules <- [[{<<"values">>, [<<"required">>, null]}], [{<<"keys">>, [[<<"required">>, []]]}],
                  [{<<"when">>, [<<"t">>, [{<<"c">>, [null]}]]}]]],
    %% LIVR 2.0
    ?assertEqual({ok, [{<<"x">>, null}]}, olifer:validate([{}], [{<<"x">>, [{<<"default">>, null}]}])),
    ok = olifer:register_rule(<<"echo_args">>, new_rules, echo_args),
    ?assertEqual(ok, olifer:check_schema([{<<"x">>, [{<<"echo_args">>, null}]}])),
    ?assertEqual({ok, [{<<"x">>, {args, null}}]},
                 olifer:validate([{<<"x">>, <<"a">>}], [{<<"x">>, [{<<"echo_args">>, null}]}])).

%% An unknown rule (or alias) is a schema error, anywhere in the rules: check_schema/1 returns it,
%% validate/2 raises it
test_unknown_rules(_Config) ->
    Message = <<"Rule 'not_registered_rule' is not registered">>,
    [begin
         ?assertEqual({error, Message}, olifer:check_schema(Rules)),
         ?assertError({schema_error, Message}, olifer:validate([{<<"a">>, 1}, {<<"b">>, [{<<"k">>, 1}]}], Rules))
     end || Rules <- [[{<<"a">>, <<"not_registered_rule">>}],
                      [{<<"a">>, [<<"required">>, <<"not_registered_rule">>]}],
                      [{<<"b">>, [{<<"values">>, <<"not_registered_rule">>}]}],
                      [{<<"b">>, [{<<"nested_object">>, [{<<"k">>, <<"not_registered_rule">>}]}]}],
                      %% also for a field that is not in the data
                      [{<<"c">>, <<"not_registered_rule">>}]]],
    ok = olifer:register_aliased_rule([[{<<"name">>, <<"broken_alias">>}, {<<"rules">>, <<"not_registered_rule">>}]]),
    ?assertError({schema_error, Message}, olifer:validate([{<<"a">>, 1}], [{<<"a">>, <<"broken_alias">>}])),
    %% a rule is known once it is registered
    Rules = [{<<"a">>, <<"later_registered_rule">>}],
    ?assertError({schema_error, _}, olifer:validate([{<<"a">>, 1}], Rules)),
    ok = olifer:register_rule(<<"later_registered_rule">>, new_rules, echo_args),
    ?assertEqual({ok, [{<<"a">>, {args, []}}]}, olifer:validate([{<<"a">>, 1}], Rules)).

%% INTERNAL
test_2_5_suite(Suite, Type, ExpectedFile) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_2_5_TEST_PATH ++ Suite ++ "/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         DataDir = WholePath ++ TestDir,
         ok = register_border_aliases(DataDir),
         Rules = read_json(DataDir, "rules.json"),
         Input = read_json(DataDir, "input.json"),
         Expected = read_json(DataDir, ExpectedFile),
         Result = with_options(DataDir, fun() -> olifer:validate(Input, Rules) end),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nExpected: ~p,~nResult: ~p~n",
             [TestDir, Rules, Input, Expected, Result]),
         ?assertEqual(unordered({Type, Expected}), unordered(Result))
     end || TestDir <- lists:sort(ListDir)].

%% Every schema must fail the schema check, with its aliases registered
test_schema_errors(WholePath) ->
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         DataDir = WholePath ++ TestDir,
         ok = register_border_aliases(DataDir),
         Rules = read_json(DataDir, "rules.json"),
         Result = olifer:check_schema(Rules),
         ct:print("Test name:~p~nRules: ~p~nResult: ~p~n", [TestDir, Rules, Result]),
         ?assertMatch({error, _}, Result)
     end || TestDir <- lists:sort(ListDir), filelib:is_dir(WholePath ++ TestDir)].

%% options.json: {"now": "2026-10-18T12:00:00Z"} is the current time of the case
with_options(DataDir, Validate) ->
    case file:read_file(DataDir ++ "/options.json") of
        {ok, Options} ->
            Now = olifer:get_value(<<"now">>, decode(Options)),
            Seconds = calendar:rfc3339_to_system_time(binary_to_list(Now)),
            ok = meck:new(olifer_clock),
            ok = meck:expect(olifer_clock, system_time, fun() -> Seconds end),
            try Validate() after meck:unload(olifer_clock) end;
        {error, enoent} ->
            Validate()
    end.

read_json(DataDir, File) ->
    {ok, Json} = file:read_file(DataDir ++ "/" ++ File),
    decode(Json).

%% aliases.json of a border test is optional
register_border_aliases(DataDir) ->
    case file:read_file(DataDir ++ "/aliases.json") of
        {ok, Aliases} -> olifer:register_aliased_rule(decode(Aliases));
        {error, enoent} -> ok
    end.

%% JSON objects are unordered: compare them as maps
unordered({Type, Data}) ->
    {Type, unordered(Data)};
unordered([{}]) ->
    #{};
unordered([{Key, _} | _] = PropList) when is_binary(Key) ->
    maps:from_list([{K, unordered(V)} || {K, V} <- PropList]);
unordered(List) when is_list(List) ->
    [unordered(V) || V <- List];
unordered(Value) ->
    Value.

get_main_path() ->
    _ = shell_default:cd(".."),
    _ = shell_default:cd(".."),
    {ok, MainPath} = file:get_cwd(),
    MainPath.

decode(BinaryData) ->
    try
        jsx:decode(BinaryData)
    catch
        _:_ -> json_parsing_error
    end.

get_working_data(positive, TestDir, Path) ->
    DataDir = Path ++ "/" ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {decode(Rules), decode(Input), decode(Output)};
get_working_data(negative, TestDir, Path) ->
    DataDir = Path ++ "/" ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {decode(Rules), decode(Input), decode(Errors)};
get_working_data(aliases_positive, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {decode(Aliases), decode(Rules), decode(Input), decode(Output)};
get_working_data(aliases_negative, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {decode(Aliases), decode(Rules), decode(Input), decode(Errors)};
get_working_data(register_positive, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {decode(Rules), decode(Input), decode(Output)};
get_working_data(register_negative, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {decode(Rules), decode(Input), decode(Errors)};
get_working_data(border_suite_positive, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {decode(Rules), decode(Input), decode(Output)};
get_working_data(border_suite_negative, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {decode(Rules), decode(Input), decode(Errors)}.

