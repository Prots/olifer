#DESCRIPTION

Olifer is lightweight validator supporting Language Independent Validation Rules Specification (LIVR) for Erlang

See http://livr-spec.org for detailed documentation and list of supported rules.

[![Build Status](https://travis-ci.org/Prots/olifer.svg?branch=master)](https://travis-ci.org/Prots/olifer)

**Features:**

* Rules are declarative and language independent
* Any number of rules for each field
* Return together errors for all fields
* Excludes all fields that do not have validation rules described
* Has possibility to validatate complex hierarchical structures
* Easy to describe and undersand rules
* Returns understandable error codes(not error messages)
* Easy to add own rules
* Rules are be able to change results output ("trim", "nested_object", for example)
* Multipurpose (user input validation, configs validation, contracts programming etc)
 
#GETTING STARTED
1. Add as a dependency in your project:
  * For **rebar** add to rebar.config
   ```erl
      {olifer, ".*",
           {git, "git@github.com:Prots/olifer.git", {branch, master}}
      }
   ```
  * For **erlang.mk** add to make file:
   ```erl
      DEPS = olifer
      dep_olifer = git@github.com:Prots/olifer.git master
   ```
2. Add in **your_project.app.src** file in tuple **applications**:
   ```erl  
   {applications, [
                   kernel,
                   stdlib,
                   olifer
                   ]
     }
   ```
3. Run **olifer:start()** in your project start function.
4. Thats all, now you can validate data, register your own rules or aliased built-in rules.
 
#USAGE
**1. Validate data**

Simple example:
```erl
1> Input =  [{<<"first_name">>,<<"Vasya">>}].

2> Rule = [{<<"first_name">>,[{<<"length_between">>,[4,6]}]}].

3> olifer:validate(Input, Rule).

4> {ok, [{<<"first_name">>,<<"Vasya">>}]}

5> Input1 =  [{<<"number1">>,-1.12}].

6> Rule1 = [{<<"number1">>,<<"integer">>}].

7> olifer:validate(Input1, Rule1).

8> {errors, [{<<"number1">>,<<"NOT_INTEGER">>}]}
```
More complex example
```erl
1> Input =  [{<<"address">>,
                [{<<"country">>, <<"Ukraine">>},
                 {<<"zip">>, <<"12345">>},
                 {<<"street">>, <<"10">>},
                 {<<"building">>, <<"10">>},
                 {<<"extra_field">>, <<"will be removed">>}]},
            {<<"extra_field">>, <<"will be removed">>}].

2> Rules = [{<<"address">>,
                [<<"required">>,
                 [{<<"nested_object">>,
                   [{<<"country">>, [<<"required">>, [{<<"one_of">>, [[<<"Ukraine">>, <<"USA">>]]}]]},
                    {<<"zip">>, <<"positive_integer">>},
                    {<<"street">>, <<"required">>},
                    {<<"building">>, [<<"required">>, <<"positive_integer">>]}]}]]}].

3> olifer:validate(Input, Rules).

4> {ok, [{<<"address">>,
           [{<<"country">>, <<"Ukraine">>},
            {<<"zip">>, <<"12345">>},
            {<<"street">>, <<"10">>},
            {<<"building">>, <<"10">>}]}]}
```
**2. Register aliased rule**

The **"name"** and **"rules"** fields is required, **"error"** is not required for alias definition
```erl
1> Alias1 = [[{<<"name">>, <<"adult_age">>},
              {<<"rules">>, [<<"positive_integer">>, [{<<"min_number">>, 18}]]}]].

2> Alias2 = [[{<<"name">>, <<"adult_age_with_custom_error">>},
              {<<"rules">>, [<<"positive_integer">>, [{<<"min_number">>, 18}]]},
              {<<"error">>, <<"WRONG_AGE">>}]].

3> olifer:register_aliased_rule(Alias1).
ok
4> olifer:register_aliased_rule(Alias2).
ok
5> Rules =  [{<<"age1">>, <<"adult_age">>}, {<<"age2">>, <<"adult_age_with_custom_error">>}].

6> Input = [{<<"age1">>, 14}].

7> Input1 = [{<<"age1">>, 32}].

8> Input2 = [{<<"age2">>, 15}].

9> olifer:validate(Input, Rules).
{errors, [{<<"age1">>,<<"TOO_LOW">>}]}

10> olifer:validate(Input1, Rules).
{ok, [{<<"age1">>, 32}]}

11> olifer:validate(Input2, Rules).
{errors, [{<<"age2">>, <<"WRONG_AGE">>}]}
```
**3. Register new rule**

You can create your own rules, for example:
```erl
-module(new_rules).

-export([strong_password/2]).

strong_password(Value, [MinLength]) ->
    strong_password(Value, MinLength);
strong_password(Value, MinLength) when is_binary(Value), byte_size(Value) >= MinLength ->
    {ok, Value};
strong_password(Value, _Args) when is_binary(Value) ->
    {error, <<"WEAK_PSSSWORD">>};
strong_password(_Value, _Args) ->
    {error, <<"FORMAT_ERROR">>}.
```
The function that describe the rule should has 2 arguments, first argument is **Value** for validation and second argument is a value/list of values for rule.
After creation your own rule you should register it:
```erl
1> olifer:register_rule(<<"strong_password">>, new_rules, strong_password).
ok
2> Rules = [{<<"password1">>, [{<<"strong_password">>, 15}]}, {<<"password2">>, [{<<"strong_password">>,[15]}]}].

3> Input = [{<<"password1">>, <<"123456789012345">>}, {<<"password2">>, <<"asdasdasdasdasdasd">>}].

4> olifer:validate(Input, Rules).
{ok, [{<<"password1">>, <<"123456789012345">>}, {<<"password2">>, <<"asdasdasdasdasdasd">>}]}

5> Input1 = [{<<"password1">>, <<"1234">>}, {<<"password2">>, <<"asdasd">>}].

6> olifer:validate(Input1, Rules).
{errors, [{<<"password1">>, <<"WEAK_PSSSWORD">>}, {<<"password2">>, <<"WEAK_PSSSWORD">>}]}
```
