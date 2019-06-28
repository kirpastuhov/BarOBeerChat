%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Июнь 2019 6:10
%%%-------------------------------------------------------------------
-module(bobc_authorization_window).
-author("User").

%% API
-export([show_window/4]).

show_window(ServerAddress, ServerPort, LocalAddress, LocalPort) ->

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
                  bobc_connection_window:show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                {in_use} ->
                  io:format("This username is in use, try another~n"),
                  show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
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
                  bobc_connection_window:show_window(Username, ServerAddress, ServerPort, LocalAddress, LocalPort);

                {not_found} ->
                  io:format("This username is not found~n"),
                  show_window(ServerAddress, ServerPort, LocalAddress, LocalPort);

                {wrong_password} ->
                  io:format("Wrong password~n"),
                  show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
              end

          after 2000 ->
            io:format("Server is not responding, but you can try once more~n"),
            show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
          end;

        {error, _Reason} -> io:format("Connection failed, but you could try again~n"),
          show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
      end;

    ["exit"] -> ok;
    _ ->
      io:format("Unknown command~n"),
      show_window(ServerAddress, ServerPort, LocalAddress, LocalPort)
  end.


