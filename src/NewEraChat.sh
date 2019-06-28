#!/bin/bash
erlc bobc_crypto.erl
erlc bobc_utils.erl
erlc bobc_net.erl
erlc bobc_gen_server.erl
erlc bobc_client.erl
erlc bobc_connection_window.erl
erlc bobc_authorization_window.erl
erl -noshell -sname Alex -s bobc_client start 4200 localhost 5555