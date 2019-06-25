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


%% API
-export([start/1]).

-export([init/0, writer_loop/0, reader_loop/2, tcp_receiver_loop/3, accept/3, handle_connection/3]).

start([UsernameArg, PortArg, RemoteNodeName, AddressArg, RemotePortArg, PublicKeyArg]) ->

  Username = atom_to_list(UsernameArg),
  LocalPort = list_to_integer(atom_to_list(PortArg)),
  RemoteUsername = atom_to_list(RemoteNodeName),
  RemoteAddress = atom_to_list(AddressArg),
  RemotePort = list_to_integer(atom_to_list(RemotePortArg)),
  RemotePublicKey = atom_to_list(PublicKeyArg),
  LocalAddress = "localhost",

  main(Username, [{RemoteUsername,RemoteAddress, RemotePort, RemotePublicKey}, {Username,LocalAddress, LocalPort}]);


start([UsernameArg, PortArg]) ->
  Username = atom_to_list(UsernameArg),
  LocalPort = list_to_integer(atom_to_list(PortArg)),
  LocalAddress = "localhost",

  main(Username, [{Username,LocalAddress, LocalPort}]).


main(Username, Clients) ->
  init(),
%% Process that handle incoming messages
  WriterPid = spawn_link(?MODULE, writer_loop, []),


  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Paul Generate Public and Private Keys %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  {PublicKey, PrivateKey} = generateKeys(),


  [ThisClient | RestClients] = lists:reverse(Clients),
  {ClientName, LocalAddress, LocalPort} = ThisClient,
  ThisNewClient = {ClientName, LocalAddress, LocalPort, PublicKey},

  NewClientList = lists:reverse([ThisNewClient | RestClients]),

%% Gen_server
  {ok, ServerPid} = chat_server:start_link({Username, WriterPid, NewClientList}),

  %%prints local history (for a start)
  print_history(WriterPid),



  spawn_link(?MODULE, tcp_receiver_loop, [ServerPid, LocalPort, PrivateKey]),

%% Process that handle input
  reader_loop(ServerPid, WriterPid),

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