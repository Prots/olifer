PROJECT = olifer

DEPS = jsx
TEST_DEPS = LIVR meck

dep_jsx       = git https://github.com/talentdeficit/jsx.git        v2.6.2
dep_LIVR      = git https://github.com/koorchik/LIVR                master
dep_meck      = git https://github.com/eproxus/meck.git             0.9.2

TEST_DIR = test

include erlang.mk
