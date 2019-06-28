%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Июнь 2019 1:33
%%%-------------------------------------------------------------------
-module(bobc_client).
-author("User").

-include_lib("stdlib/include/qlc.hrl").

-record(message, {msg_id, name, message}).


%% API
-export([start/1, main/4]).

-export([writer_loop/0, reader_loop/3, handle_connection/1]).

start([PortArg, ServerAddressArg, ServerPortArg]) ->

  ServerAddress = atom_to_list(ServerAddressArg),
  ServerPort = list_to_integer(atom_to_list(ServerPortArg)),
  LocalPort = list_to_integer(atom_to_list(PortArg)),
  LocalAddress = "localhost",

  io:format(os:cmd(clear)),
  io:format("Welcome to the BarOBeerChat!~n~n"),
  io:format("Type register <Username> <Password> to sign up~nType login <Username> <Password> to sign in~n~n"),


  bobc_authorization_window:show_window(ServerAddress, ServerPort, LocalAddress, LocalPort),

  io:format("~nBye!~n"),

  init:stop().


main(Username, ChatId, Clients, PrivateKey) ->
  init_database(ChatId),
%% Process that handle incoming messages
  WriterPid = spawn_link(?MODULE, writer_loop, []),

  [ThisClient | _] = lists:reverse(Clients),
  {_, _, LocalPort, _} = ThisClient,

%% Gen_server
  {ok, ServerPid} = bobc_gen_server:start_link({Username, WriterPid, Clients}),

  %%prints local history (for a start)
  print_history(ChatId, WriterPid),


  spawn_link(bobc_net, tcp_listener_loop, [LocalPort, [?MODULE, handle_connection, ServerPid, PrivateKey, ChatId]]),

%% Process that handle input
  reader_loop(ServerPid, WriterPid, ChatId).

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
reader_loop(ServerPid, WriterPid, ChatId) ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> gen_server:call(ServerPid, shutdown);
    _ -> gen_server:call(ServerPid, {send, Input}),
      io:format(os:cmd(clear)),
      print_history(ChatId, WriterPid),
      reader_loop(ServerPid, WriterPid, ChatId)
  end.


handle_connection([Socket | [ServerPid | [PrivateKey | [ChatId]]]]) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Msg} ->
      Input = binary_to_term(Msg),
      case Input of
        {message, Username, Text} ->
          gen_server:call(ServerPid, {print, Username, Text, PrivateKey, ChatId});
        {join, {Username, Address, Port, PublicKey}} ->
          gen_server:call(ServerPid, {joined, {Username, Address, Port, PublicKey}});
        {left, {Username, LocalAddress, LocalPort, PublicKey}} ->
          gen_server:call(ServerPid, {left, {Username, LocalAddress, LocalPort, PublicKey}});
        {client_list, Clients} ->
          gen_server:call(ServerPid, {join, Clients})
      end,
      gen_tcp:close(Socket);

    {error, closed} ->
      io:format("Lost connection~n"),
      gen_tcp:close(Socket);
    {error, timeout} ->
      io:format("Lost connection~n"),
      gen_tcp:close(Socket)

  end.


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.


print_history(ChatId, WriterPid) ->
  mnesia:start(),
  mnesia:wait_for_tables([list_to_atom(ChatId)], 20000),
  Messages = do(qlc:q([{X#message.name, X#message.message} || X <- mnesia:table(list_to_atom(ChatId))])),
  Fun = fun(Message) ->
    {Username, Msg} = Message,
    WriterPid ! {message, {Username, Msg}}
        end,
  lists:map(Fun, Messages).

init_database(ChatId) ->
  case mnesia:create_schema([node()]) of
    {error,
      {_, {already_exists, _}}} ->
      ok;
    ok -> ok
  end,
  mnesia:start(),
  case mnesia:create_table(list_to_atom(ChatId),
    [{attributes, record_info(fields, message)},
      {disc_copies, [node()]}, {type, ordered_set},
      {record_name, message}])
  of
    {aborted, {already_exists, _}} -> ok;
    {atomic, ok} ->
      Row = #message{name = system, message = "Welcome to chat " ++ ChatId, msg_id = 0},
      F = fun() -> mnesia:write(list_to_atom(ChatId),Row, write) end,
      mnesia:transaction(F),
      ok
  end,
  mnesia:wait_for_tables([list_to_atom(ChatId)], 20000).