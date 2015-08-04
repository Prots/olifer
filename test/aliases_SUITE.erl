-module(aliases_SUITE).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-include("olifer_test.hrl").
-include("olifer.hrl").

-export([all/0]).
-export([test_positive/1, test_negative/1]).
-export([init_per_suite/1, end_per_suite/1]).

all() ->
    [
        test_positive,
        test_negative
    ].

init_per_suite(Config) ->
    ok = olifer:start(),
    Config.

end_per_suite(Config) ->
    ok = olifer:stop(),
    Config.

%% TESTS
test_positive(_Config) ->
    _ = shell_default:cd(".."),
    _ = shell_default:cd(".."),
    {ok, MainPath} = file:get_cwd(),
    WholePath = MainPath ++ "/" ++ ?TEST_PATH ++ "aliases_positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Output} = get_working_data(positive, TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p, Rules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Aliases, Rules, Input, Output]),
         ok = olifer:register_aliased_rule(Aliases),
         Result = [{Field#field.name, Field#field.output} || Field <- olifer:validate(Input, Rules)],
         ?assertEqual(olifer:decode(Output), Result)
     end || TestDir <- lists:sort(ListDir)].


test_negative(_Config) ->
    _ = shell_default:cd(".."),
    _ = shell_default:cd(".."),
    {ok, MainPath} = file:get_cwd(),
    WholePath = MainPath ++ "/" ++ ?TEST_PATH ++ "aliases_negative/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Errors} = get_working_data(negative, TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p, Rules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Aliases, Rules, Input, Errors]),
         ok = olifer:register_aliased_rule(Aliases),
         Result = [{Field#field.name, Field#field.errors} || Field <- olifer:validate(Input, Rules), Field#field.errors =/= []],
         ?assertEqual(olifer:decode(Errors), Result)
     end || TestDir <- lists:sort(ListDir)].

%% INTERNAL

get_working_data(positive, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {Aliases, Rules, Input, Output};
get_working_data(negative, TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {Aliases, Rules, Input, Errors}.