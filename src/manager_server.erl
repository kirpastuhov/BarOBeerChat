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

-record(chatroom, {chat_id :: term(), client = [] :: term()}).

-record(user, {login :: term(), password :: term()}).


%% API
-export([start/1]).

-export([tcp_receiver_loop/1, accept/1, handle_connection/1]).


start([PortArg]) ->

  Port = list_to_integer(atom_to_list(PortArg)),

  init_database(),

  spawn_link(?MODULE, tcp_receiver_loop, [Port]),

  reader_loop(),

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
          connect_user_to_chat(ChatId, Client);
        {connect, ChatId, Client} ->
          Val = check_chat_if_exists(ChatId),
          if
            Val -> connect_user_to_chat(ChatId, Client);
            true -> send_term(Client, {not_found})
          end;
        {login, Login, Password, Client} ->
          Status = check_login(Login, Password),
          case Status of
            {ok} -> send_term(Client, {success});
            {not_found} -> send_term(Client, {not_found});
            {wrong_password} -> send_term(Client, {wrong_password})
          end;
        {register, Login, Password, Client} ->
          Status = register_new_user(Login, Password),
          case Status of
            {ok} -> send_term(Client, {success});
            {login_in_use} -> send_term(Client, {in_use})
          end

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

  Val = check_chat_if_exists(ChatId),

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Kirill Сохранение ид чата в бд  %%
  %%Не забудь проверить, что такого id нет%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ChatId.

connect_user_to_chat(ChatId, {Username, Address, Port}) ->

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Kirill Добавлять активного юзера в список юзеров для токена %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Kirill Возвращать список/одного активного юзера чата    %%
  %% Записывай его в переменную User ниже; User = {Address, Port} %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  User = {},

  send_term({Address, Port}, {connect, User}).

check_chat_if_exists(ChatId) ->

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Kirill Проверять если такой чат в бд %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  Val = true,
  Val.


generate_chat_id() ->

     ChatId = base64:encode(crypto:strong_rand_bytes(6)),

  ChatId.

check_login(Login, Password) ->

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Kirill Получить логин и пароль из бд %%
  %% User = {Login, Password}                  %%
  %% Если нет, в статус записывай {not_found}  %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  User = {},


  {_, DBPassword} = User,
  Status = if
             Password /= DBPassword -> {wrong_password};
             true -> {ok}
           end,
  Status.


register_new_user(Login, Password) ->

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% TODO Kirill Вносить в бд нового юзера  %%
  %% Перед этим проверять не занят ли логин %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  Status = {ok}, %% или {login_in_use}

  Status.


reader_loop() ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> ok;
    _ -> reader_loop()
  end.


send_term({Address, Port}, Term) ->
  {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {active, true}, {packet, raw}]),
  gen_tcp:send(Socket, term_to_binary(Term)),
  gen_tcp:close(Socket).