#!/bin/bash

erl -pa ebin/ deps/*/ebin test/ -boot start_sasl -s vffov
