-module(olifer_app).
-author("prots.igor@gmail.com").

-behaviour(application).

-include("olifer.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks
start(_StartType, _StartArgs) ->
    ok = init(),
    olifer_sup:start_link().

stop(_State) ->
    ok.

%% Internal

init() ->
    _ = ets:new(?RULES_TBL, [public, set, named_table, {read_concurrency, true}]),
    _ = ets:new(?ALIASES_TBL, [public, set, named_table, {read_concurrency, true}]),
    ok.