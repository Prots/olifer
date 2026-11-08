-module(olifer_formats).

-include("olifer.hrl").

%% The format rules of LIVR 2.5. They check the string form of a string or a number and output it.
%% A boolean gets the error of the rule, an object or a list FORMAT_ERROR

-define(HEX, "[0-9a-fA-F]").
-define(IPV4, "(?:(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.){3}"
              "(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])").

%% API
-export([
    uuid/3,
    ip/3,
    cidr/3,
    hostname/3,
    base64/3,
    hex/3,
    digest/3,
    mongo_id/3,
    hex_color/3,
    mac/3,
    e164/3,
    semver/3,
    jwt/3
]).
%% For the checksum rules and the schema check
-export([check_format/3, matches/2]).
-export([uuid_version/1, digest_length/1]).

%% API
uuid(Value, Args, _) ->
    check_format(Value, ?WRONG_UUID, fun(String) ->
        case [uuid_version(Version) || Version <- olifer:args(Args)] of
            [] ->
                matches(String, "^" ?HEX "{8}-" ?HEX "{4}-[1-8]" ?HEX "{3}-[89abAB]" ?HEX "{3}-" ?HEX "{12}$")
                    orelse matches(String, "^(?:00000000-0000-0000-0000-000000000000"
                                           "|[fF]{8}-[fF]{4}-[fF]{4}-[fF]{4}-[fF]{12})$");
            Versions ->
                matches(String, "^" ?HEX "{8}-" ?HEX "{4}-[1-8]" ?HEX "{3}-[89abAB]" ?HEX "{3}-" ?HEX "{12}$")
                    andalso lists:member(binary:at(String, 14), Versions)
        end
    end).

ip(Value, Args, _) ->
    check_format(Value, ?WRONG_IP, fun(String) ->
        Version = ip_version(Args),
        (Version =/= v6 andalso is_ipv4(String)) orelse (Version =/= v4 andalso is_ipv6(String))
    end).

%% An address, "/" and a prefix length without leading zeros. Host bits may be set
cidr(Value, Args, _) ->
    check_format(Value, ?WRONG_CIDR, fun(String) ->
        Version = ip_version(Args),
        case binary:split(String, <<"/">>) of
            [Address, Prefix] ->
                MaxPrefix = case Version =/= v6 andalso is_ipv4(Address) of
                    true -> 32;
                    false ->
                        case Version =/= v4 andalso is_ipv6(Address) of
                            true -> 128;
                            false -> -1
                        end
                end,
                matches(Prefix, "^(?:0|[1-9][0-9]{0,2})$") andalso binary_to_integer(Prefix) =< MaxPrefix;
            _ ->
                false
        end
    end).

%% RFC 1123 labels, at most 253 characters, the last label is not all digits
hostname(Value, _Args, _) ->
    check_format(Value, ?WRONG_HOSTNAME, fun(String) ->
        byte_size(String) =< 253
            andalso matches(String, "^[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                                    "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$")
            andalso not matches(String, "(?:^|\\.)[0-9]+$")
    end).

%% "base64": the standard alphabet with padding, {"base64": "url"}: the URL-safe alphabet,
%% the padding is optional
base64(Value, Args, _) ->
    check_format(Value, ?WRONG_BASE64, fun(String) ->
        case olifer:args(Args) of
            [] -> matches(String, "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$");
            _ -> matches(String, "^(?:[A-Za-z0-9_-]{4})*(?:[A-Za-z0-9_-]{2}(?:==)?|[A-Za-z0-9_-]{3}=?)?$")
        end
    end).

hex(Value, _Args, _) ->
    check_format(Value, ?WRONG_HEX, fun(String) -> matches(String, "^" ?HEX "+$") end).

digest(Value, Args, _) ->
    check_format(Value, ?WRONG_DIGEST, fun(String) ->
        lists:member(byte_size(String), [digest_length(Algorithm) || Algorithm <- olifer:args(Args)])
            andalso matches(String, "^" ?HEX "+$")
    end).

mongo_id(Value, _Args, _) ->
    check_format(Value, ?WRONG_MONGO_ID, fun(String) -> matches(String, "^" ?HEX "{24}$") end).

hex_color(Value, _Args, _) ->
    check_format(Value, ?WRONG_HEX_COLOR, fun(String) ->
        matches(String, "^#(?:" ?HEX "{3,4}|" ?HEX "{6}|" ?HEX "{8})$")
    end).

%% Six (48 bits, the default) or eight (64 bits) pairs, separated by ":" or by "-"
mac(Value, Args, _) ->
    check_format(Value, ?WRONG_MAC, fun(String) ->
        Bits = case olifer:args(Args) of
            [] -> 48;
            [Arg | _] -> olifer_schema:non_negative_integer(Arg)
        end,
        Pairs = integer_to_list(Bits div 8 - 2),
        matches(String, ["^" ?HEX "{2}([:-])" ?HEX "{2}(?:\\1" ?HEX "{2}){", Pairs, "}$"])
    end).

e164(Value, _Args, _) ->
    check_format(Value, ?WRONG_E164, fun(String) -> matches(String, "^\\+[1-9][0-9]{6,14}$") end).

%% Semantic Versioning 2.0.0, the regular expression of semver.org with ASCII classes
semver(Value, _Args, _) ->
    check_format(Value, ?WRONG_SEMVER, fun(String) ->
        matches(String, "^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)"
                        "(?:-((?:0|[1-9][0-9]*|[0-9]*[a-zA-Z-][0-9a-zA-Z-]*)"
                        "(?:\\.(?:0|[1-9][0-9]*|[0-9]*[a-zA-Z-][0-9a-zA-Z-]*))*))?"
                        "(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$")
    end).

%% Three base64url segments without padding (the signature may be empty), no segment has
%% a length of 1 modulo 4. The segments are not decoded
jwt(Value, _Args, _) ->
    check_format(Value, ?WRONG_JWT, fun(String) ->
        matches(String, "^[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]*$")
            andalso lists:all(fun(Segment) -> byte_size(Segment) rem 4 =/= 1 end,
                              binary:split(String, <<".">>, [global]))
    end).

%% Empty values pass, a string or a number is checked in its string form, which is the output
check_format(Value, _Error, _IsValid) when Value =:= <<>>; Value =:= null ->
    {ok, Value};
check_format(Value, Error, IsValid) when is_binary(Value); is_number(Value) ->
    String = olifer:to_binary(Value),
    case IsValid(String) of
        true when String =:= Value -> {ok, Value};
        true -> {filter, String};
        false -> {error, Error}
    end;
check_format(Value, Error, _IsValid) when is_boolean(Value) ->
    {error, Error};
check_format(_Value, _Error, _IsValid) ->
    {error, ?FORMAT_ERROR}.

%% The whole value matches: a trailing newline does not match $ (dollar_endonly).
%% No unicode option: the patterns are ASCII, other bytes do not match them
matches(String, Pattern) ->
    re:run(String, Pattern, [dollar_endonly, {capture, none}]) =:= match.

%% The version digit of "v1" ... "v8"
uuid_version(<<"v", Digit>>) when Digit >= $1, Digit =< $8 -> Digit;
uuid_version(_Version) -> error.

%% The number of hex digits of a digest
digest_length(<<"md5">>) -> 32;
digest_length(<<"sha1">>) -> 40;
digest_length(<<"sha256">>) -> 64;
digest_length(<<"sha384">>) -> 96;
digest_length(<<"sha512">>) -> 128;
digest_length(_Algorithm) -> error.

%% INTERNAL
ip_version(Args) ->
    case olifer:args(Args) of
        [<<"v4">> | _] -> v4;
        [<<"v6">> | _] -> v6;
        [] -> any
    end.

is_ipv4(String) ->
    matches(String, "^" ?IPV4 "$").

%% Eight groups of 1-4 hex digits, one "::" for one or more groups of zeros, an IPv4 address
%% in the last two groups
is_ipv6(String) ->
    case binary:split(String, <<"::">>, [global]) of
        [Address] ->
            ipv6_groups(Address, true) =:= 8;
        [Before, After] ->
            BeforeGroups = ipv6_groups(Before, false),
            AfterGroups = ipv6_groups(After, true),
            BeforeGroups >= 0 andalso AfterGroups >= 0 andalso BeforeGroups + AfterGroups =< 7;
        _ ->
            false
    end.

%% The number of groups, -1 for a wrong group
ipv6_groups(<<>>, _IsAddressEnd) ->
    0;
ipv6_groups(Address, IsAddressEnd) ->
    Groups = binary:split(Address, <<":">>, [global]),
    {Init, [Last]} = lists:split(length(Groups) - 1, Groups),
    IsGroup = fun(Group) -> matches(Group, "^" ?HEX "{1,4}$") end,
    case lists:all(IsGroup, Init) of
        false -> -1;
        true ->
            case IsGroup(Last) of
                true -> length(Groups);
                false when IsAddressEnd ->
                    case is_ipv4(Last) of
                        true -> length(Groups) + 1;
                        false -> -1
                    end;
                false -> -1
            end
    end.
