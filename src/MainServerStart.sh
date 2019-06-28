#!/bin/bash
erlc bobc_net.erl
erlc bobc_main_server.erl
erl -noshell -sname server -s bobc_main_server start 5555