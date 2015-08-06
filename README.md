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
```erl
1> Input =  [{<<"address">>,
                [{<<"country">>,<<"Ukraine">>},
                 {<<"zip">>,<<"12345">>},
                 {<<"street">>,<<"10">>},
                 {<<"building">>,<<"10">>},
                 {<<"extra_field">>,<<"will be removed">>}]},
            {<<"extra_field">>,<<"will be removed">>}].

2> Rules = [{<<"address">>,
                [<<"required">>,
                 [{<<"nested_object">>,
                   [{<<"country">>,[<<"required">>,[{<<"one_of">>,[[<<"Ukraine">>,<<"USA">>]]}]]},
                    {<<"zip">>,<<"positive_integer">>},
                    {<<"street">>,<<"required">>},
                    {<<"building">>,[<<"required">>,<<"positive_integer">>]}]}]]}].

3> olifer:validate(Input, Rules).

4> [{<<"address">>,
       [{<<"country">>,<<"Ukraine">>},
        {<<"zip">>,<<"12345">>},
        {<<"street">>,<<"10">>},
        {<<"building">>,<<"10">>}]}]
```
**2. Register aliased rule**

**3. Register new rule**

