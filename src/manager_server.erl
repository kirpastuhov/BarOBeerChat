%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Июнь 2019 0:14
%%%-------------------------------------------------------------------
-module(manager_server).
-author("User").

-include_lib("stdlib/include/qlc.hrl").

-import(utils, [generate_chars_and_numbers/1]).

-record(chatroom, {chat_id :: term(), client = [] :: term()}).

-record(user, {login :: term(), password :: term()}).


%% API
-export([start/1]).

-export([tcp_receiver_loop/1, accept/1, handle_connection/1]).


start([PortArg]) ->
  Port = list_to_integer(atom_to_list(PortArg)),

  init_database(),

  spawn_link(?MODULE, tcp_receiver_loop, [Port]),

  io:format("Server successfully started~nType 'exit' to stop it~n"),

  reader_loop(),

  io:format("Server is stopping~n"),

  init:stop().


init_database() ->
  case mnesia:create_schema([node()]) of
    {error,
      {_, {already_exists, _}}} ->
      ok;
    ok -> ok
  end,

  mnesia:start(),

  case mnesia:create_table(chatroom,
    [{attributes, record_info(fields, chatroom)},
      {disc_copies, [node()]}, {type, ordered_set}])
  of
    {aborted, {already_exists, chatroom}} -> ok;
    {atomic, ok} -> ok
  end,

  case mnesia:create_table(user,
    [{attributes, record_info(fields, user)},
      {disc_copies, [node()]}, {type, ordered_set}])
  of
    {aborted, {already_exists, user}} -> ok;
    {atomic, ok} -> ok
  end,

  mnesia:wait_for_tables([chatroom, user], 20000).


tcp_receiver_loop(Port) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
  spawn(?MODULE, accept, [ListenSocket]),
  timer:sleep(infinity),
  ok.


accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(?MODULE, handle_connection, [Socket]),
  accept(ListenSocket).


handle_connection(Socket) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Msg} ->
      Input = binary_to_term(Msg),
      case Input of

        {create, Client} ->
          ChatId = create_chatroom(),
          gen_tcp:send(Socket, term_to_binary(connect_user_to_chat(ChatId, Client))),
          {Username, _, _, _} = Client,
          io:format("~s created new chat - ~s~n", [Username, ChatId]);
        {connect, ChatId, Client} ->
          Val = check_chat_if_exists(ChatId),
          if
            Val ->
              gen_tcp:send(Socket, term_to_binary(connect_user_to_chat(ChatId, Client))),
              {Username, _, _, _} = Client,
              io:format("~s connected to chat ~s~n", [Username, ChatId]);
            true ->
              gen_tcp:send(Socket, term_to_binary({not_found}))
          end;
        {login, Login, Password} ->
          Status = check_login(Login, Password),
          case Status of
            {ok} ->
              io:format("~s logged in~n", [Login]),
              gen_tcp:send(Socket, term_to_binary({success}));
            {not_found} -> gen_tcp:send(Socket, term_to_binary({not_found}));
            {wrong_password} -> gen_tcp:send(Socket, term_to_binary({wrong_password}))
          end;
        {register, Login, Password} ->
          Status = register_new_user(Login, Password),
          case Status of
            {ok} ->
              io:format("~s registered~n", [Login]),
              gen_tcp:send(Socket, term_to_binary({success}));
            {login_in_use} -> gen_tcp:send(Socket, term_to_binary({in_use}))
          end;
        {left, Client, ChatId} ->
          delete_client_from_chat(Client, ChatId),
          {Username, _, _, _} = Client,
          io:format("~s left the chat ~s~n", [Username, ChatId])
      end,
      gen_tcp:close(Socket);

    {error, closed} ->
      io:format("Connection closed~n"),
      gen_tcp:close(Socket);
    {error, timeout} ->
      io:format("Timeout~n"),
      gen_tcp:close(Socket)

  end.


create_chatroom() ->
  ChatId = generate_chat_id(),
  Row = #chatroom{chat_id = ChatId},
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F),
  ChatId.


connect_user_to_chat(ChatId, {Username, Address, Port, PublicKey}) ->
  [{_, UsrList}] = do(qlc:q([{X#chatroom.chat_id, X#chatroom.client} || X <- mnesia:table(chatroom), X#chatroom.chat_id =:= ChatId])),

  NewUser = {Username, Address, Port, PublicKey},
  Val = lists:member(NewUser, UsrList),

  NewUsrList = if
                 Val -> UsrList;
                 true -> UsrList ++ [NewUser]
               end,

  Row = #chatroom{chat_id = ChatId, client = NewUsrList},          % Сохраняем его в бд
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F),
  if
    length(UsrList) =:= 0 ->
      {connect, ChatId};
    true -> User = hd(UsrList),
      {connect, ChatId, [User]}
  end.

delete_client_from_chat(Client, ChatId) ->
  [{_, UsrList}] = do(qlc:q([{X#chatroom.chat_id, X#chatroom.client} || X <- mnesia:table(chatroom), X#chatroom.chat_id =:= ChatId])),
  Val = lists:member(Client, UsrList),
  NewUserList =
    if
      Val ->
        lists:delete(Client, UsrList);
      true ->
        UsrList
    end,
  Row = #chatroom{chat_id = ChatId, client = NewUserList},          % Сохраняем его в бд
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F).



check_chat_if_exists(ChatId) ->
  Res = do(qlc:q([X#chatroom.chat_id || X <- mnesia:table(chatroom), X#chatroom.chat_id =:= ChatId])),
  Val = if
          Res /= [ChatId] -> false;
          true -> true
        end,
  Val.


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.


generate_chat_id() ->
  ChatId = utils:generate_chars_and_numbers(6),
  case check_chat_if_exists(ChatId) of
    true -> generate_chat_id();
    false -> ChatId
  end.


check_login(Login, Password) ->
  User = do(qlc:q([{X#user.login, X#user.password} || X <- mnesia:table(user),
    X#user.login =:= Login])),
  Status = if
             User =:= [] -> {not_found};
             true ->
               [{_, DBPassword}] = User,
               if
                 Password /= DBPassword -> {wrong_password};
                 true -> {ok}
               end
           end,
  Status.


register_new_user(Login, Password) ->
  DBLogin = do(qlc:q([{X#user.login} || X <- mnesia:table(user), X#user.login =:= Login])),
  if
    DBLogin =:= [{Login}] -> Status = {login_in_use};
    true -> Status = {ok},
      Row = #user{login = Login, password = Password},
      F = fun() -> mnesia:write(Row) end,
      mnesia:transaction(F)
  end,
  Status.


reader_loop() ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> ok;
    _ -> reader_loop()
  end.
