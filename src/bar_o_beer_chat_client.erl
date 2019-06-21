%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Июнь 2019 1:33
%%%-------------------------------------------------------------------
-module(bar_o_beer_chat_client).
-author("User").

-import(chat_server, [start_link/1]).

%% API
-export([start/1]).

-export([writer_loop/0, reader_loop/1, fantom_writer/1]).

start([UsernameArg]) ->

  Username = atom_to_list(UsernameArg),
%% Запуск райтера
  WriterPid = spawn_link(?MODULE, writer_loop, []),
%% Запуск ген сервера
  {ok, ServerPid} = chat_server:start_link({Username, WriterPid}),
%% Запуск фантома
  spawn_link(?MODULE, fantom_writer, [ServerPid]),
%% Запуск ридера
  reader_loop(ServerPid),
  init:stop().

writer_loop() ->
  receive
    {message, {Username, Message}} -> io:format("<~s> ~s~n", [Username, Message]),
      writer_loop();
    shutdown -> ok;
    Message -> io:format("Unknown message: ~p~n", [Message]),
      writer_loop()
  end.

reader_loop(ServerPid) ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> gen_server:call(ServerPid, shutdown);
    _ -> gen_server:call(ServerPid, {send, Input}),
      reader_loop(ServerPid)
  end.


fantom_writer(ServerPid) ->
  timer:sleep(10000),
  gen_server:call(ServerPid, {print, "Phantom", "Ha-ha-ha"}),
  fantom_writer(ServerPid).