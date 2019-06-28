#!/bin/bash
erlc bobc_crypto.erl
erlc bobc_utils.erl
erlc bobc_net.erl
erlc bobc_gen_server.erl
erlc bobc_client.erl
erl -noshell -sname Alex -s bobc_client start 4200 localhost 5555