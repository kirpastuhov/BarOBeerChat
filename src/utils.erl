%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Июнь 2019 3:46
%%%-------------------------------------------------------------------
-module(utils).
-author("User").

%% API
-export([send_term/2]).


send_term({Address, Port}, Term) ->
  case gen_tcp:connect(Address, Port, [binary, {active, true}, {packet, raw}]) of
    {ok, Socket} ->
      gen_tcp:send(Socket, term_to_binary(Term)),
      {ok, Socket};
    {error, Reason} -> {error, Reason}
  end.
