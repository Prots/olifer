-module(main_SUITE).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-include("olifer.hrl").

-define(LIVR_TEST_PATH, "deps/LIVR/test_suite/").
-define(CT_TEST_PATH, "test/register_suite/").

-export([all/0]).
-export([test_positive/1, test_negative/1]).
-export([test_aliases_positive/1, test_aliases_negative/1]).
-export([test_register_positive/1, test_register_negative/1]).
-export([init_per_suite/1, end_per_suite/1]).

all() ->
    [
        test_positive,
        test_negative,
        test_aliases_positive,
        test_aliases_negative,
        test_register_positive,
        test_register_negative
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
         {Rules, Input, Output} = get_working_data(positive, TestDir, WholePath),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Rules, Input, Output]),
         Result = [{Field#field.name, Field#field.output} || Field <- olifer:validate(Input, Rules)],
         ?assertEqual(decode(Output), Result)
     end || TestDir <- lists:sort(ListDir)].

test_negative(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Errors} = get_working_data(negative, TestDir, WholePath),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nErrors: ~p,~n", [TestDir, Rules, Input, Errors]),
         Result = [{Field#field.name, Field#field.errors} || Field <- olifer:validate(Input, Rules), Field#field.errors =/= []],
         ?assertEqual(decode(Errors), Result)
     end || TestDir <- lists:sort(ListDir)].

test_aliases_positive(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "aliases_positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Output} = get_working_data(aliases_positive, TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p, Rules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Aliases, Rules, Input, Output]),
         ok = olifer:register_aliased_rule(Aliases),
         Result = [{Field#field.name, Field#field.output} || Field <- olifer:validate(Input, Rules)],
         ?assertEqual(decode(Output), Result)
     end || TestDir <- lists:sort(ListDir)].

test_aliases_negative(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?LIVR_TEST_PATH ++ "aliases_negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Errors} = get_working_data(aliases_negative, TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p, Rules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Aliases, Rules, Input, Errors]),
         ok = olifer:register_aliased_rule(Aliases),
         Result = [{Field#field.name, Field#field.errors} || Field <- olifer:validate(Input, Rules), Field#field.errors =/= []],
         ?assertEqual(decode(Errors), Result)
     end || TestDir <- lists:sort(ListDir)].

test_register_positive(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?CT_TEST_PATH ++ "positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Output} = get_working_data(register_positive, TestDir, WholePath),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Rules, Input, Output]),
         ok = olifer:register_rule(list_to_binary(TestDir), new_rules, list_to_atom(TestDir)),
         Result = [{Field#field.name, Field#field.output} || Field <- olifer:validate(Input, Rules)],
         ?assertEqual(decode(Output), Result)
     end || TestDir <- lists:sort(ListDir)].

test_register_negative(_Config) ->
    WholePath = get_main_path() ++ "/" ++ ?CT_TEST_PATH ++ "negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Rules, Input, Errors} = get_working_data(register_negative, TestDir, WholePath),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Rules, Input, Errors]),
         ok = olifer:register_rule(list_to_binary(TestDir), new_rules, list_to_atom(TestDir)),
         Result = [{Field#field.name, Field#field.errors} || Field <- olifer:validate(Input, Rules)],
         ?assertEqual(decode(Errors), Result)
     end || TestDir <- lists:sort(ListDir)].

%% INTERNAL
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
    {Rules, Input, Output};
get_working_data(negative, TestDir, Path) ->
    DataDir = Path ++ "/" ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {Rules, Input, Errors};
get_working_data(aliases_positive, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {Aliases, Rules, Input, Output};
get_working_data(aliases_negative, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {Aliases, Rules, Input, Errors};
get_working_data(register_positive, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {Rules, Input, Output};
get_working_data(register_negative, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {Rules, Input, Errors}.
