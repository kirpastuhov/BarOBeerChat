#!/bin/bash
erlc utils.erl
erlc chat_server.erl
erlc bar_o_beer_chat_client.erl
erl -noshell -sname Kirill -s bar_o_beer_chat_client start 1234 localhost 5555