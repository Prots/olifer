-module(negative).
-author("prots.igor@gmail.com").

-include_lib("eunit/include/eunit.hrl").
-include("olifer_test.hrl").
-include("olifer.hrl").

%% TESTS
-export([all_test/0]).

%% TESTS
all_test() ->
    {ok, ListDir} = file:list_dir(?TEST_PATH ++ atom_to_list(?MODULE)),
    [begin
         {Rules, Input, Errors} = get_working_data(TestDir),
         ct:print("Test name:~p~nRules: ~p,~nInput: ~p,~nErrors: ~p,~n", [TestDir, Rules, Input, Errors]),
         Result = [{Field#field.name, Field#field.errors} || Field <- olifer:validate(Input, Rules), Field#field.errors =/= []],
         ?assertEqual(olifer:decode(Errors), Result)
     end || TestDir <- lists:sort(ListDir)].

%% INTERNAL

get_working_data(TestDir) ->
    DataDir = ?TEST_PATH ++ atom_to_list(?MODULE) ++ "/" ++ TestDir,
    {ok, Rules} = file:read_file(DataDir ++ "/rules.json"),
    {ok, Input} = file:read_file(DataDir ++ "/input.json"),
    {ok, Errors} = file:read_file(DataDir ++ "/errors.json"),
    {Rules, Input, Errors}.
