%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Июнь 2019 3:08
%%%-------------------------------------------------------------------
-module(bobc_crypto).
-author("User").

%% API
-export([generateKeys/0, encrypt_message/2, decrypt_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generates Public and Private Keys for RSA encryption %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generateKeys() ->
  {_PublicKey, _PrivateKey} = crypto:generate_key(rsa, {2048, 257}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns bitstring encrypted with PublicKey using RSA %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encrypt_message(Message, PublicKey) ->
  crypto:public_encrypt(rsa, list_to_binary(Message), PublicKey, rsa_pkcs1_padding).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decrypts the Message using PrivateKey %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decrypt_message(Message, PrivateKey) ->
  binary_to_list(crypto:private_decrypt(rsa, Message, PrivateKey, rsa_pkcs1_padding)).