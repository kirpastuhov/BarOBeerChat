#!/bin/bash
erlc utils.erl
erlc chat_server.erl
erlc bar_o_beer_chat_client.erl
erl -noshell -sname Alex -s bar_o_beer_chat_client start 4200 localhost 5555