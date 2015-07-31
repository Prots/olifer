#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin \  -sname olifer -s olifer
