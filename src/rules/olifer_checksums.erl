-module(olifer_checksums).

-include("olifer.hrl").

%% The checksum rules of LIVR 2.5. The string form is checked as it is: no spaces or dashes
%% are removed. A boolean gets the error of the rule, an object or a list FORMAT_ERROR

%% API
-export([
    luhn/3,
    credit_card/3,
    iban/3
]).

%% API
luhn(Value, _Args, _) ->
    olifer_formats:check_format(Value, ?WRONG_CHECKSUM, fun(String) ->
        olifer_formats:matches(String, "^[0-9]{2,}$") andalso has_luhn_checksum(String)
    end).

%% 12-19 digits (ISO/IEC 7812) and the Luhn checksum
credit_card(Value, _Args, _) ->
    olifer_formats:check_format(Value, ?WRONG_CREDIT_CARD_NUMBER, fun(String) ->
        olifer_formats:matches(String, "^[0-9]{12,19}$") andalso has_luhn_checksum(String)
    end).

%% ISO 13616: the country, two check digits and 11-30 letters and digits, the number modulo 97 is 1
iban(Value, _Args, _) ->
    olifer_formats:check_format(Value, ?WRONG_IBAN, fun(String) ->
        olifer_formats:matches(String, "^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$") andalso has_iban_checksum(String)
    end).

%% INTERNAL
%% Every second digit from the right is doubled
has_luhn_checksum(Digits) ->
    {Sum, _} = lists:foldl(fun(Char, {Acc, IsDoubled}) ->
        Digit = case IsDoubled of
            true when Char - $0 > 4 -> (Char - $0) * 2 - 9;
            true -> (Char - $0) * 2;
            false -> Char - $0
        end,
        {Acc + Digit, not IsDoubled}
    end, {0, false}, lists:reverse(binary_to_list(Digits))),
    Sum rem 10 =:= 0.

%% The first four characters move to the end, a letter is two digits: A = 10 ... Z = 35
has_iban_checksum(<<First:4/binary, Rest/binary>>) ->
    Remainder = lists:foldl(fun
        (Char, Acc) when Char =< $9 -> (Acc * 10 + Char - $0) rem 97;
        (Char, Acc) -> (Acc * 100 + Char - $A + 10) rem 97
    end, 0, binary_to_list(<<Rest/binary, First/binary>>)),
    Remainder =:= 1.
