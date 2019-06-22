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

-export([writer_loop/0, reader_loop/1, fantom_writer/1, tcp_receiver_loop/2, accept/2, handle_connection/2]).

start([UsernameArg, PortArg]) ->

  Username = atom_to_list(UsernameArg),
  Port = list_to_integer(atom_to_list(PortArg)),

%% Process that handle incoming messages
  WriterPid = spawn_link(?MODULE, writer_loop, []),

%% Gen_server
  {ok, ServerPid} = chat_server:start_link({Username, WriterPid}),

  %%TODO fun that prints local history (for a start)
  print_history(WriterPid),

  spawn_link(?MODULE, tcp_receiver_loop, [ServerPid, Port]),

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

tcp_receiver_loop(ServerPid, Port) ->

  %% Must send messages to ServerPid in following format: gen_server:call(ServerPid, {print, Username, Message})

  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
  spawn(?MODULE, accept, [ListenSocket, ServerPid]),
  timer:sleep(infinity),
  ok.



accept(ListenSocket, ServerPid) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, handle_connection, [Socket, ServerPid]),
  accept(ListenSocket, ServerPid).

handle_connection(Socket, ServerPid) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Msg} -> {Username, Text} = binary_to_term(Msg),
      gen_server:call(ServerPid, {print, Username, Text}),

      gen_tcp:close(Socket);

    {error, closed} ->
      io:format("error"),
      gen_tcp:close(Socket);
    {error, timeout} ->
      io:format("Socket , session closed by timeout ~n"),
      gen_tcp:close(Socket)

  end.


print_history(WriterPid) ->
  %% You get the list of messages from db and print them
  %% It's better to do it like: WriterPid ! {message, {Username, Message}}.
  ok.