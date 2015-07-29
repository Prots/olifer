PROJECT = olifer

DEPS = jsx parselib
TEST_DEPS = LIVR

dep_jsx       = git https://github.com/talentdeficit/jsx.git        v2.6.2
dep_parselib  = git https://github.com/erlangbureau/parselib.git    v2.1.0
dep_LIVR      = git https://github.com/koorchik/LIVR                master

TEST_DIR = test

include erlang.mk
