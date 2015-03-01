#!/bin/bash

erl -pa $(pwd)/ebin -pa $(pwd)/deps/*/ebin -name eric@127.0.0.1 -setcookie eric -s monkey
