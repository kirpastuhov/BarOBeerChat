%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Июнь 2019 0:14
%%%-------------------------------------------------------------------
-module(bobc_main_server).
-author("User").

-include_lib("stdlib/include/qlc.hrl").

-import(utils, [generate_chars_and_numbers/1]).

%% Definition of the chatroom table
-record(chatroom, {chat_id :: term(), client = [] :: [term()]}).

%% Definition of the user table
-record(user, {login :: term(), password :: term()}).


%% API
-export([start/1]).

%% Local functions
-export([handle_connection/1]).

%% Main function
start([PortArg]) ->
  Port = list_to_integer(atom_to_list(PortArg)),

  init_database(),

  %% bobc_net function that listens for incoming connections
  %% and sends them to defined function of this module
  spawn_link(bobc_net, tcp_listener_loop, [Port, {?MODULE, handle_connection}]),

  io:format("Server successfully started~nType 'exit' to stop it~n"),

  reader_loop(),

  io:format("Server is stopping~n"),

  init:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creates schema and two databases: User and Chatroom if they don't exist %%
%%                                                                         %%
%% Table user is responsible for store users authentication information    %%
%%                                                                         %%
%% Table chatroom stores list of active users of each chatroom and provide %%
%% it to users that want to connect to certain chat                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function that handle incoming connections               %%
%% Socket to this function comes from bobc_net module      %%
%% All you need is to define expected requests and actions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_connection(Socket) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Msg} ->
      Input = binary_to_term(Msg),
      case Input of

        %% Expected when user wants to create a new chat
        {create, Client} ->
          ChatId = create_chatroom(),
          gen_tcp:send(Socket, term_to_binary(connect_user_to_chat(ChatId, Client))),
          {Username, _, _, _} = Client,
          io:format("~s created new chat - ~s~n", [Username, ChatId]);

        %% Expected when user wants to connect to existing chat
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

        %% Expected when user wants to authorise
        {login, Login, Password} ->
          Status = check_login(Login, Password),
          case Status of
            {ok} ->
              io:format("~s logged in~n", [Login]),
              gen_tcp:send(Socket, term_to_binary({success}));
            {not_found} -> gen_tcp:send(Socket, term_to_binary({not_found}));
            {wrong_password} -> gen_tcp:send(Socket, term_to_binary({wrong_password}))
          end;

        %% Expected when user wants to register
        {register, Login, Password} ->
          Status = register_new_user(Login, Password),
          case Status of
            {ok} ->
              io:format("~s registered~n", [Login]),
              gen_tcp:send(Socket, term_to_binary({success}));
            {login_in_use} -> gen_tcp:send(Socket, term_to_binary({in_use}))
          end;

        %% Expected when user lefts the chat
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate new chatId and add it to database %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_chatroom() ->
  ChatId = generate_chat_id(),
  Row = #chatroom{chat_id = ChatId},
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F),
  ChatId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gets active users of the chat and form the answer %%
%% to user, if he need to connect to someone         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_user_to_chat(ChatId, {Username, Address, Port, PublicKey}) ->
  [{_, UsrList}] = do(qlc:q([{X#chatroom.chat_id, X#chatroom.client} || X <- mnesia:table(chatroom), X#chatroom.chat_id =:= ChatId])),

  NewUser = {Username, Address, Port, PublicKey},
  FixedUsrList = delete_user_by_username(Username, UsrList),
  NewUsrList = FixedUsrList ++ [NewUser],

  Row = #chatroom{chat_id = ChatId, client = NewUsrList},
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F),
  if
    length(FixedUsrList) =:= 0 ->
      {connect, ChatId};
    true -> User = hd(FixedUsrList),
      {connect, ChatId, [User]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Deletes user from the list of active users %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_client_from_chat(Client, ChatId) ->
  [{_, UsrList}] = do(qlc:q([{X#chatroom.chat_id, X#chatroom.client} || X <- mnesia:table(chatroom), X#chatroom.chat_id =:= ChatId])),
  {Username, _, _, _} = Client,
  NewUserList = delete_user_by_username(Username, UsrList),
  Row = #chatroom{chat_id = ChatId, client = NewUserList},
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns true if the chat with this chatId exists, false - if not %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_chat_if_exists(ChatId) ->
  Res = do(qlc:q([X#chatroom.chat_id || X <- mnesia:table(chatroom), X#chatroom.chat_id =:= ChatId])),
  Val = if
          Res /= [ChatId] -> false;
          true -> true
        end,
  Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function-helper that returns the result_set of the query Q %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generates unique string of chars and number %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_chat_id() ->
  ChatId = utils:generate_chars_and_numbers(6),
  case check_chat_if_exists(ChatId) of
    true -> generate_chat_id();
    false -> ChatId
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks if login exists and the password is correct %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks if login is already in use and add it to database is not %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Process that handles input into terminal %%
%% Responsible for shutdown the server      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reader_loop() ->
  Input = string:strip(io:get_line(""), both, $\n),
  case Input of
    "exit" -> ok;
    _ -> reader_loop()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Deletes all users with name "Username" from the list "Users" %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_user_by_username(Username, Users) ->
  F = fun(User, Acc) ->
    {Name, _, _, _} = User,
    NewAcc = if
               Name /= Username -> Acc ++ [User];
               true -> Acc
             end,
    NewAcc
      end,
  lists:foldl(F, [], Users).