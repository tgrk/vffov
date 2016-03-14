#!/bin/bash

erl -pa ebin/ deps/*/ebin -d -boot start_sasl -noshell -detached -s vffov
