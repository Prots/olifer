-module(aliases_positive_SUITE).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-include("olifer_test.hrl").
-include("olifer.hrl").

-export([all/0]).
-export([test_all/0]).
-export([init_per_suite/1, end_per_suite/1]).

all() ->
    [
        test_all
    ].

init_per_suite(Config) ->
    ok = olifer:start(),
    Config.

end_per_suite(Config) ->
    ok = olifer:stop(),
    Config.

%% TESTS
test_all() ->
    _ = shell_default:cd(".."),
    _ = shell_default:cd(".."),
    {ok, MainPath} = file:get_cwd(),
    WholePath = MainPath ++ "/" ++ ?TEST_PATH ++ "aliases_positive/",
    {ok, ListDir} = file:list_dir(WholePath),
    [begin
         {Aliases, Rules, Input, Output} = get_working_data(TestDir, WholePath),
         ct:print("Test name:~p~nAliases:~p, Rules: ~p,~nInput: ~p,~nOutput: ~p,~n", [TestDir, Aliases, Rules, Input, Output]),
         ok = olifer:register_aliased_rule(Aliases),
         Result = [{Field#field.name, Field#field.errors} || Field <- olifer:validate(Input, Rules), Field#field.errors =/= []],
         ct:print("Result: ~p", [Result]),
         ?assertEqual(olifer:decode(Output), Result)
     end || TestDir <- lists:sort(ListDir)].

%% INTERNAL

get_working_data(TestDir, Path) ->
    DataDir = Path ++ TestDir,
    {ok, Aliases} = file:read_file(DataDir ++ "/aliases.json"),
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Output} = file:read_file(DataDir ++ "/output.json"),
    {Aliases, Rules, Input, Output}.