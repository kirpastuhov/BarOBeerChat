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
-export([send_term/2, generate_chars_and_numbers/1]).


send_term({Address, Port}, Term) ->
  case gen_tcp:connect(Address, Port, [binary, {active, true}, {packet, raw}]) of
    {ok, Socket} ->
      gen_tcp:send(Socket, term_to_binary(Term)),
      {ok, Socket};
    {error, Reason} -> {error, Reason}
  end.


generate_chars_and_numbers(Length) -> generate_random_string(Length, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").

generate_random_string(Length, AllowedChars) ->
  MaxLength = length(AllowedChars),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(rand:uniform(MaxLength), AllowedChars)] ++ Acc end,
    [], lists:seq(1, Length)
  ).