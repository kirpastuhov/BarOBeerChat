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
-export([start/1]).

-export([init/1, writer_loop/0, reader_loop/3, tcp_receiver_loop/4, accept/4, handle_connection/4]).

start([PortArg, ServerAddressArg, ServerPortArg]) ->

  ServerAddress = atom_to_list(ServerAddressArg),
  ServerPort = list_to_integer(atom_to_list(ServerPortArg)),
  LocalPort = list_to_integer(atom_to_list(PortArg)),
  LocalAddress = "localhost",

  io:format(os:cmd(clear)),
  io:format("Welcome to the BarOBeerChat!~n~n"),
  io:format("Type register <Username> <Password> to sign up~nType login <Username> <Password> to sign in~n~n"),


  authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort),

  io:format("~nBye!~n"),

  init:stop().

authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort) ->

  Input = string:strip(io:get_line(""), both, $\n),
  List = string:split(Input, " ", all),

  case List of


    ["register", Username, Password] ->
      io:format("Connecting to server...~n"),
      case bobc_utils:send_term({ServerAddress, ServerPort}, {register, Username, Password}) of

        {ok, _} ->

          receive

            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {success} ->
                  io:format(os:cmd(clear)),
                  io:format("Hello, ~s!~n~nTo create new chat type 'create'~nTo connect to existing one type 'connect <chat_id>'~n~n", [Username]),
                  connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                {in_use} ->
                  io:format("This username is in use, try another~n"),
                  authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
      end;


    ["login", Username, Password] ->
      io:format("Connecting to server...~n"),
      case bobc_utils:send_term({ServerAddress, ServerPort}, {login, Username, Password}) of

        {ok, _} ->

          receive
            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {success} ->
                  io:format(os:cmd(clear)),
                  io:format("Hello, ~s!~n~nTo create new chat type 'create'~nTo connect to existing one type 'connect <chat_id>'~n~n", [Username]),
                  connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                {not_found} ->
                  io:format("This username is not found~n"),
                  authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort);

                {wrong_password} ->
                  io:format("Wrong password~n"),
                  authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
      end;

    ["exit"] -> ok;
    _ ->
      io:format("Unknown command~n"),
      authorization_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
  end.

connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort) ->



  {PublicKey, PrivateKey} = bobc_crypto:generateKeys(),

  ThisUser = {Username, LocalAddress, LocalPort, PublicKey},

  Input = string:strip(io:get_line(""), both, $\n),
  List = string:split(Input, " ", all),

  case List of

    ["create"] ->
      io:format("Connecting to server...~n"),
      case bobc_utils:send_term({ServerAddress, ServerPort}, {create, ThisUser}) of

        {ok, _} ->

          receive
            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {connect, ChatId} ->
                  io:format(os:cmd(clear)),
                  main(Username, ChatId, [{Username, LocalAddress, LocalPort, PublicKey}], PrivateKey),
                  bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, ChatId})

              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
      end;


    ["connect", ChatId] ->
      io:format("Connecting to server...~n"),
      case bobc_utils:send_term({ServerAddress, ServerPort}, {connect, ChatId, ThisUser}) of

        {ok, _} ->

          receive
            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {connect, GotChatId, RemoteUsers} ->
                  [{RemoteUsername, Address, Port, _} | _] = RemoteUsers,
                  io:format("Connecting to ~s - ~p:~p~n", [RemoteUsername, Address, Port]),
                  io:format(os:cmd(clear)),
                  main(Username, GotChatId, lists:reverse([{Username, LocalAddress, LocalPort, PublicKey}] ++ RemoteUsers), PrivateKey),
                  bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, GotChatId});

                {connect, GotChatId} ->
                  io:format(os:cmd(clear)),
                  main(Username, GotChatId, [{Username, LocalAddress, LocalPort, PublicKey}], PrivateKey),
                  bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, GotChatId});

                {not_found} ->
                  io:format("Chat not found~n"),
                  connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                Reply ->
                  io:format("Reply: ~p~n", [Reply])

              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
      end;


    ["exit", ChatId] -> bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, ChatId}),
      connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

    ["exit"] -> ok;
    _ ->
      io:format("Unknown command~n"),
      connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
  end.

main(Username, ChatId, Clients, PrivateKey) ->
  init(ChatId),
%% Process that handle incoming messages
  WriterPid = spawn_link(?MODULE, writer_loop, []),

  [ThisClient | _] = lists:reverse(Clients),
  {_, _, LocalPort, _} = ThisClient,

%% Gen_server
  {ok, ServerPid} = bobc_gen_server:start_link({Username, WriterPid, Clients}),

  %%prints local history (for a start)
  print_history(ChatId, WriterPid),


  spawn_link(?MODULE, tcp_receiver_loop, [ServerPid, LocalPort, PrivateKey, ChatId]),

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


tcp_receiver_loop(ServerPid, Port, PrivateKey, ChatId) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
  spawn(?MODULE, accept, [ListenSocket, ServerPid, PrivateKey, ChatId]),
  timer:sleep(infinity),
  ok.



accept(ListenSocket, ServerPid, PrivateKey, ChatId) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, handle_connection, [Socket, ServerPid, PrivateKey, ChatId]),
  accept(ListenSocket, ServerPid, PrivateKey, ChatId).

handle_connection(Socket, ServerPid, PrivateKey, ChatId) ->
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
      io:format("error"),
      gen_tcp:close(Socket);
    {error, timeout} ->
      io:format("Socket , session closed by timeout ~n"),
      gen_tcp:close(Socket)

  end.


do(Q, WriterPid) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Fun = fun(Message) ->
    {Username, Msg} = Message,
    WriterPid ! {message, {Username, Msg}}
        end,
  lists:map(Fun, Val).


print_history(ChatId, WriterPid) ->
  mnesia:start(),
  mnesia:wait_for_tables([list_to_atom(ChatId)], 20000),
  do(qlc:q([{X#message.name, X#message.message} || X <- mnesia:table(list_to_atom(ChatId))]), WriterPid).

init(ChatId) ->
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