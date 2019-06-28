%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Июнь 2019 3:46
%%%-------------------------------------------------------------------
-module(bobc_utils).
-author("User").


%% API
-export([send_term/2, get_difference/2, generate_chars_and_numbers/1, delete_user_by_username/2]).


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the difference between to lists %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_difference(List1, List2) ->
  F = fun(Item, DifferenceList) ->
    Val = lists:member(Item, List2),
    if Val -> DifferenceList;
      true -> [Item | DifferenceList]
    end
      end,
  F2 = fun(Item, DifferenceList) ->
    Val = lists:member(Item, List1),
    if Val -> DifferenceList;
      true -> [Item | DifferenceList]
    end
       end,
  lists:foldl(F2, [], List2) ++ lists:foldl(F, [], List1).

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