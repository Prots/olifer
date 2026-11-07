-author("prots.igor@gmail.com").

%% missing: the field is not in the data and no rule has given it a value yet
-record(field, {name, input, rules, output, errors, missing = false}).

-define(RULES_TBL, olifer_user_rules).
-define(ALIASES_TBL, olifer_user_aliases).
%% The schemas that passed the schema check
-define(SCHEMAS_TBL, olifer_checked_schemas).
-define(MAX_SCHEMAS, 1000).

%% Whitespace of JavaScript's trim() and \s, for regular expressions with the unicode option
-define(WHITESPACE, "\\t\\n\\x{0B}\\f\\r \\x{A0}\\x{1680}\\x{2000}-\\x{200A}\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}").

-define(SPEC_RULES, [<<"required">>, <<"not_empty_list">>, <<"default">>, <<"or">>,
                     <<"required_if">>, <<"when">>]).

%% The standard rules skip empty values, null too. These rules check null or pass it on
-define(SEES_NULL(Rule), (Rule =:= <<"required">> orelse Rule =:= <<"not_empty">>
                          orelse Rule =:= <<"not_empty_list">> orelse Rule =:= <<"default">>
                          orelse Rule =:= <<"or">> orelse Rule =:= <<"when">>
                          orelse Rule =:= <<"any">> orelse Rule =:= <<"required_if">>)).

%% BASIC RULES ERRORS
-define(REQUIRED, <<"REQUIRED">>).
-define(CANNOT_BE_EMPTY, <<"CANNOT_BE_EMPTY">>).
-define(NOT_BOOLEAN, <<"NOT_BOOLEAN">>).

%% NUMBER RULES ERRORS
-define(NOT_NUMBER, <<"NOT_NUMBER">>).
-define(NOT_INTEGER, <<"NOT_INTEGER">>).
-define(NOT_POSITIVE_INTEGER, <<"NOT_POSITIVE_INTEGER">>).
-define(NOT_DECIMAL, <<"NOT_DECIMAL">>).
-define(NOT_POSITIVE_DECIMAL, <<"NOT_POSITIVE_DECIMAL">>).
-define(TOO_HIGH, <<"TOO_HIGH">>).
-define(TOO_LOW, <<"TOO_LOW">>).
-define(TOO_MANY_DECIMALS, <<"TOO_MANY_DECIMALS">>).

%% STRING RULES ERRORS
-define(NOT_ALLOWED_VALUE, <<"NOT_ALLOWED_VALUE">>).
-define(TOO_LONG, <<"TOO_LONG">>).
-define(TOO_SHORT, <<"TOO_SHORT">>).
-define(WRONG_FORMAT, <<"WRONG_FORMAT">>).
-define(NOT_DIGITS, <<"NOT_DIGITS">>).

%% SPECIAL RULES ERRORS
-define(WRONG_EMAIL, <<"WRONG_EMAIL">>).
-define(WRONG_URL, <<"WRONG_URL">>).
-define(WRONG_DATE, <<"WRONG_DATE">>).
-define(FIELDS_NOT_EQUAL, <<"FIELDS_NOT_EQUAL">>).
-define(DATE_TOO_EARLY, <<"DATE_TOO_EARLY">>).
-define(DATE_TOO_LATE, <<"DATE_TOO_LATE">>).
-define(WRONG_DATETIME, <<"WRONG_DATETIME">>).
-define(DATETIME_TOO_EARLY, <<"DATETIME_TOO_EARLY">>).
-define(DATETIME_TOO_LATE, <<"DATETIME_TOO_LATE">>).
-define(WRONG_TIME, <<"WRONG_TIME">>).
-define(TIME_TOO_EARLY, <<"TIME_TOO_EARLY">>).
-define(TIME_TOO_LATE, <<"TIME_TOO_LATE">>).

%% METARULES ERRORS
-define(WRONG_KEY, <<"WRONG_KEY">>).

%% LIST RULES ERRORS
-define(TOO_FEW_ITEMS, <<"TOO_FEW_ITEMS">>).
-define(TOO_MANY_ITEMS, <<"TOO_MANY_ITEMS">>).
-define(NOT_UNIQUE_ITEMS, <<"NOT_UNIQUE_ITEMS">>).

%% FORMAT RULES ERRORS
-define(WRONG_UUID, <<"WRONG_UUID">>).
-define(WRONG_IP, <<"WRONG_IP">>).
-define(WRONG_CIDR, <<"WRONG_CIDR">>).
-define(WRONG_HOSTNAME, <<"WRONG_HOSTNAME">>).
-define(WRONG_BASE64, <<"WRONG_BASE64">>).
-define(WRONG_HEX, <<"WRONG_HEX">>).
-define(WRONG_DIGEST, <<"WRONG_DIGEST">>).
-define(WRONG_MONGO_ID, <<"WRONG_MONGO_ID">>).
-define(WRONG_HEX_COLOR, <<"WRONG_HEX_COLOR">>).
-define(WRONG_MAC, <<"WRONG_MAC">>).
-define(WRONG_E164, <<"WRONG_E164">>).
-define(WRONG_SEMVER, <<"WRONG_SEMVER">>).
-define(WRONG_JWT, <<"WRONG_JWT">>).

%% CHECKSUM RULES ERRORS
-define(WRONG_CHECKSUM, <<"WRONG_CHECKSUM">>).
-define(WRONG_CREDIT_CARD_NUMBER, <<"WRONG_CREDIT_CARD_NUMBER">>).
-define(WRONG_IBAN, <<"WRONG_IBAN">>).

%% OTHER
-define(FORMAT_ERROR, <<"FORMAT_ERROR">>).

