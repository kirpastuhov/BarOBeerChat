%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Июнь 2019 6:06
%%%-------------------------------------------------------------------
-module(bobc_connection_window).
-author("User").

%% API
-export([show_window/5]).

show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort) ->



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
                  bobc_client:main(Username, ChatId, [{Username, LocalAddress, LocalPort, PublicKey}], PrivateKey),
                  bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, ChatId}),
                  io:format(os:cmd(clear)),
                  io:format("Hello, ~s!~n~nTo create new chat type 'create'~nTo connect to existing one type 'connect <chat_id>'~n~n", [Username]),
                  show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)

              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
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
                  bobc_client:main(Username, GotChatId, lists:reverse([{Username, LocalAddress, LocalPort, PublicKey}] ++ RemoteUsers), PrivateKey),
                  bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, GotChatId}),
                  io:format(os:cmd(clear)),
                  io:format("Hello, ~s!~n~nTo create new chat type 'create'~nTo connect to existing one type 'connect <chat_id>'~n~n", [Username]),
                  show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                {connect, GotChatId} ->
                  io:format(os:cmd(clear)),
                  bobc_client:main(Username, GotChatId, [{Username, LocalAddress, LocalPort, PublicKey}], PrivateKey),
                  bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, GotChatId}),
                  io:format(os:cmd(clear)),
                  io:format("Hello, ~s!~n~nTo create new chat type 'create'~nTo connect to existing one type 'connect <chat_id>'~n~n", [Username]),
                  show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                {not_found} ->
                  io:format("Chat not found~n"),
                  show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                Reply ->
                  io:format("Reply: ~p~n", [Reply])

              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
      end;


    ["exit", ChatId] -> bobc_utils:send_term({ServerAddress, ServerPort}, {left, ThisUser, ChatId}),
      show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

    ["exit"] -> ok;
    _ ->
      io:format("Unknown command~n"),
      show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort)
  end.
