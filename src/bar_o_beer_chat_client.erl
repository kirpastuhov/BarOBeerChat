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

-export([writer_loop/0, reader_loop/1, fantom_writer/1, tcp_receiver_loop/1]).

start([UsernameArg]) ->

  Username = atom_to_list(UsernameArg),

%% Process that handle incoming messages
  WriterPid = spawn_link(?MODULE, writer_loop, []),

%% Gen_server
  {ok, ServerPid} = chat_server:start_link({Username, WriterPid}),

%% TODO Process that receives messages
  spawn_link(?MODULE, tcp_receiver_loop, [ServerPid]),

%% Process that emulates someone speaks to you
  spawn_link(?MODULE, fantom_writer, [ServerPid]),

%% Process that handle input
  reader_loop(ServerPid),

  init:stop().

%%Loop that prints incoming messages
writer_loop() ->
  receive
    {message, {Username, Message}} -> io:format("<~s> ~s~n", [Username, Message]),
      writer_loop();
    shutdown -> ok;
    Message -> io:format("Unknown message: ~p~n", [Message]),
      writer_loop()
  end.

%%Loop that handle input
reader_loop(ServerPid) ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> gen_server:call(ServerPid, shutdown);
    _ -> gen_server:call(ServerPid, {send, Input}),
      reader_loop(ServerPid)
  end.

%%Fun to emulate someone is chatting with you
fantom_writer(ServerPid) ->
  timer:sleep(10000),
  gen_server:call(ServerPid, {print, "Phantom", "Ha-ha-ha"}),
  fantom_writer(ServerPid).

tcp_receiver_loop(ServerPid) ->

  %% Must send messages to ServerPid in following format: gen_server:call(ServerPid, {print, Username, Message})

  ok.