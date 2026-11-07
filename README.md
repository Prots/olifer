#DESCRIPTION

Olifer is lightweight validator supporting Language Independent Validation Rules Specification (LIVR) for Erlang

See http://livr-spec.org for detailed documentation and list of supported rules.

Olifer supports LIVR 2.5 (see **LIVR 2.5** below), so every LIVR 2.0 schema works too.

[![CI](https://github.com/Prots/olifer/actions/workflows/ci.yml/badge.svg)](https://github.com/Prots/olifer/actions/workflows/ci.yml)

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

#LIVR 2.5

LIVR 2.5 adds rules to LIVR 2.0: see the [specification](https://github.com/koorchik/LIVR/blob/master/drafts/v2.5.md),
the [changes](https://livr-spec.org/changes.html#v2-5) and the [rules](https://livr-spec.org/validation-rules.html).
Rules added in LIVR 2.5:

* Types and numbers: `boolean`, `not_one_of`, `max_decimals`
* Strings: `digits`, `starts_with`, `ends_with`, `contains`
* Dates and times: `iso_date` with `min` and `max` (`{"iso_date": {"max": "today"}}`), `iso_datetime`, `iso_time`
* Presence: `required_if`
* Metarules: `when`, `values`, `keys`, `any`
* Lists: `list_length`, `unique`
* Modifiers: `split`
* Formats: `uuid`, `ip`, `cidr`, `hostname`, `base64`, `hex`, `digest`, `mongo_id`, `hex_color`, `mac`, `e164`, `semver`, `jwt`
* Checksums: `luhn`, `credit_card`, `iban`

```erl
1> Rules = [{<<"type">>, [<<"required">>, [{<<"one_of">>, [<<"person">>, <<"company">>]}]]},
            {<<"vat">>, [{<<"required_if">>, [{<<"type">>, <<"company">>}]}]},
            {<<"subscribe">>, <<"boolean">>}].

2> olifer:validate([{<<"type">>, <<"company">>}, {<<"subscribe">>, <<"1">>}], Rules).
{errors, [{<<"vat">>, <<"REQUIRED">>}]}

3> olifer:validate([{<<"type">>, <<"person">>}, {<<"subscribe">>, <<"1">>}], Rules).
{ok, [{<<"type">>, <<"person">>}, {<<"subscribe">>, true}]}
```

**Schema errors.** `olifer:check_schema/1` checks the rules without validating data. It returns `ok`
or `{error, Message}` for an unknown rule (or alias), a wrong argument of a LIVR 2.5 rule, or an empty
list of rules or `null` in place of rules in `when`, `values` or `keys`. Call it when you build your
rules, for example in a test.
```erl
1> olifer:check_schema([{<<"id">>, [{<<"uuid">>, 4}]}]).
{error,<<"Rule 'uuid' takes versions \"v1\" to \"v8\"">>}

2> olifer:check_schema([{<<"id">>, <<"not_registered_rule">>}]).
{error,<<"Rule 'not_registered_rule' is not registered">>}
```

`olifer:validate/2` checks the rules too, before it validates the data, and raises
`error({schema_error, Message})` for these errors. An unknown rule is a schema error too: LIVR 2.0
olifer gave the field error `<<"Rule '...' not_registered">>` for it. A schema is checked once:
olifer remembers the schemas that passed the check (up to 1000) until a rule or an alias is registered.
```erl
3> olifer:validate([{}], [{<<"id">>, [{<<"uuid">>, 4}]}]).
** exception error: {schema_error,<<"Rule 'uuid' takes versions \"v1\" to \"v8\"">>}

4> olifer:validate([{<<"id">>, 1}], [{<<"id">>, <<"not_registered_rule">>}]).
** exception error: {schema_error,<<"Rule 'not_registered_rule' is not registered">>}
```

**`null` as arguments.** `{"rule": null}` is one argument `null`. No LIVR 2.5 rule takes it, so
`{"boolean": null}` is a schema error. The LIVR 2.0 rules and your own rules get `null` as before,
so `{"default": null}` sets `null`. LIVR 2.0 ignored the arguments of `iso_date`, so
`{"iso_date": null}` is `"iso_date"`.

**Other changes from LIVR 2.0.** As the LIVR specification says, the standard rules skip `null` like
an empty string, `decimal` accepts integers, `list_of` gives `FORMAT_ERROR` for an object, `or` sees
the fields next to the validated one (so `equal_to_field` works in it, as in aliases), and the rule
objects of the metarules ignore extra arguments. An unknown rule is a schema error (see above). A rule
that you register with the name of a LIVR 2.5 rule replaces it.

**The current date.** `today`, `yesterday` and `tomorrow` are the current date in UTC, from
`olifer_clock:system_time/0`. Tests fix it with [meck](https://github.com/eproxus/meck):
```erl
ok = meck:new(olifer_clock),
ok = meck:expect(olifer_clock, system_time, fun() -> calendar:rfc3339_to_system_time("2026-10-18T12:00:00Z") end),
...
meck:unload(olifer_clock).
```
