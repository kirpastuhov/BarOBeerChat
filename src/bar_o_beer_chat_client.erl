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

-include_lib("stdlib/include/qlc.hrl").

-record(message, {msg_id, name, message}).

-import(chat_server, [start_link/1]).
-import(utils, [send_term/2]).

%% API
-export([start/1]).

-export([init/0, writer_loop/0, reader_loop/2, tcp_receiver_loop/3, accept/3, handle_connection/3]).

start([PortArg, ServerAddressArg, ServerPortArg]) ->

  ServerAddress = atom_to_list(ServerAddressArg),
  ServerPort = list_to_integer(atom_to_list(ServerPortArg)),
  LocalPort = list_to_integer(atom_to_list(PortArg)),
  LocalAddress = "localhost",

  %io:format(os:cmd(clear)),
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
      case send_term({ServerAddress, ServerPort}, {register, Username, Password}) of

        {ok, _} ->

          receive

            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {success} ->
                  %io:format(os:cmd(clear)),
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
      case send_term({ServerAddress, ServerPort}, {login, Username, Password}) of

        {ok, _} ->

          receive
            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {success} ->
                  %io:format(os:cmd(clear)),
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

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Paul Generate Public and Private Keys %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  {PublicKey, PrivateKey} = generateKeys(),

  ThisUser = {Username, LocalAddress, LocalPort, PublicKey},

  Input = string:strip(io:get_line(""), both, $\n),
  List = string:split(Input, " ", all),

  case List of

    ["create"] ->
      io:format("Connecting to server...~n"),
      case send_term({ServerAddress, ServerPort}, {create, ThisUser}) of

        {ok, _} ->

          receive
            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {connect, ChatId} ->
                  io:format("Kinda got to chat ~p~n", [ChatId]),
                  %io:format(os:cmd(clear)),
                  main(Username, ChatId, [{Username, LocalAddress, LocalPort, PublicKey}], PrivateKey),
                  send_term({ServerAddress, ServerPort}, {left, ThisUser, ChatId})

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
      case send_term({ServerAddress, ServerPort}, {connect, ChatId, ThisUser}) of

        {ok, _} ->

          receive
            {tcp, _Socket, Msg} ->
              Reply = binary_to_term(Msg),

              case Reply of

                {connect, GotChatId, RemoteUser} ->
                  io:format("Kinda got to chat ~p and connected to ~p~n", [GotChatId, RemoteUser]),
                  %io:format(os:cmd(clear)),
                  main(Username, GotChatId, lists:reverse([{Username, LocalAddress, LocalPort, PublicKey}] ++ RemoteUser), PrivateKey),
                  send_term({ServerAddress, ServerPort}, {left, ThisUser, GotChatId});

                {connect, GotChatId} ->
                  io:format("Kinda got to chat ~p~n", [GotChatId]),
                  %io:format(os:cmd(clear)),
                  main(Username, GotChatId, [{Username, LocalAddress, LocalPort, PublicKey}], PrivateKey),
                  send_term({ServerAddress, ServerPort}, {left, ThisUser, GotChatId});

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


    ["exit", ChatId] -> send_term({ServerAddress, ServerPort}, {left, ThisUser, ChatId}),
      connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

    ["exit"] -> ok;
    _ ->
      io:format("Unknown command~n"),
      connection_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
  end.

main(Username, _ChatId, Clients, PrivateKey) ->
  init(),
%% Process that handle incoming messages
  WriterPid = spawn_link(?MODULE, writer_loop, []),

  [ThisClient | _] = lists:reverse(Clients),
  {_, _, LocalPort, _} = ThisClient,

%% Gen_server
  {ok, ServerPid} = chat_server:start_link({Username, WriterPid, Clients}),

  %%prints local history (for a start)
  print_history(WriterPid),


  spawn_link(?MODULE, tcp_receiver_loop, [ServerPid, LocalPort, PrivateKey]),

%% Process that handle input
  reader_loop(ServerPid, WriterPid).

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
reader_loop(ServerPid, WriterPid) ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> gen_server:call(ServerPid, shutdown);
    _ -> gen_server:call(ServerPid, {send, Input}),
%%      io:format(os:cmd(clear)),
%%      print_history(WriterPid),
      reader_loop(ServerPid, WriterPid)
  end.


tcp_receiver_loop(ServerPid, Port, PrivateKey) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
  spawn(?MODULE, accept, [ListenSocket, ServerPid, PrivateKey]),
  timer:sleep(infinity),
  ok.



accept(ListenSocket, ServerPid, PrivateKey) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, handle_connection, [Socket, ServerPid, PrivateKey]),
  accept(ListenSocket, ServerPid, PrivateKey).

handle_connection(Socket, ServerPid, PrivateKey) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Msg} ->
      Input = binary_to_term(Msg),
      case Input of
        {message, Username, Text} ->
          gen_server:call(ServerPid, {print, Username, Text, PrivateKey});
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


print_history(WriterPid) ->
  mnesia:start(),
  mnesia:wait_for_tables([message], 20000),
  do(qlc:q([{X#message.name, X#message.message} || X <- mnesia:table(message)]), WriterPid).

init() ->
  case mnesia:create_schema([node()]) of
    {error,
      {_, {already_exists, _}}} ->
      ok;
    ok -> ok
  end,
  mnesia:start(),
  case mnesia:create_table(message,
    [{attributes, record_info(fields, message)},
      {disc_copies, [node()]}, {type, ordered_set}])
  of
    {aborted, {already_exists, message}} -> ok;
    {atomic, ok} ->
      Row = #message{name = system,
        message = " start of the chat room ", msg_id = 0},
      F = fun() -> mnesia:write(Row) end,
      mnesia:transaction(F),
      ok
  end,
  mnesia:wait_for_tables([message], 20000).


generateKeys() ->
  PublicKey = "1",
  PrivateKey = "1",

  {PublicKey, PrivateKey}.