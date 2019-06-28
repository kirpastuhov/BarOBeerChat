%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июнь 2019 15:57
%%%-------------------------------------------------------------------
-module(bobc_gen_server).
-author("User").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3, found_dead_client/2]).

-define(SERVER, ?MODULE).

-record(state, {name :: term(), writer :: pid(), clients = [] :: [term()]}).
-record(message, {msg_id, name, message}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Arg :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
% start_link({Username, LastMsg, WriterPid, Clients}) ->
%   gen_server:start_link(?MODULE, [Username, LastMsg, WriterPid, Clients], []).

start_link({Username,LastMsg ,WriterPid, Clients}) ->
  gen_server:start_link(?MODULE, [Username, LastMsg, WriterPid, Clients], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Username, LastMsg, WriterPid, Clients]) ->
  %% if it isn't the only client he joins to other
  if
    length(Clients) /= 1 ->
      join(Clients, LastMsg);
    true -> ok
  end,
  {ok, #state{name = Username, writer = WriterPid, clients = Clients}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

%% Expected when this client wants to send a message
%% Calls function send_message to send message
handle_call({send, Message}, _From, State) ->
  {_, Name, _, Clients} = State,
  send_message({Name, Message, Clients}),
  {reply, ok, State};

%% Expected when got new client in the chat
%% Answered with its own clients list
handle_call({joined, Client, Messages}, _From, State) ->
  {Tag, Username, WriterPid, Clients} = State,
  {Guest, _, _, _} = Client,
  WriterPid ! {message, {Guest, "joined"}},
  send_clients_list(Client, Clients, Messages),
  NewState = {Tag, Username, WriterPid, [Client | Clients]},
  {reply, ok, NewState};

%% Expected when someone leaves the chat
%% Deletes user from its own clients list
handle_call({left, Client}, _From, State) ->
  {Tag, Username, WriterPid, Clients} = State,
  {Guest, _, _, _} = Client,
  WriterPid ! {message, {Guest, "left"}},
  NewClients = bobc_utils:delete_user_by_username(Guest, Clients),
  NewState = {Tag, Username, WriterPid, NewClients},
  {reply, ok, NewState};

%% Expected when this client got a new list of Clients to connect
%% Calls function join and adds NewClients to its state
handle_call({join, NewClients}, _From, State) ->
  {Tag, Username, WriterPid, Clients} = State,
  [ThisClient | RemoteNodes] = lists:reverse(Clients),
  DifferentClients = bobc_utils:get_difference(RemoteNodes, NewClients),
  join(lists:reverse([ThisClient | DifferentClients])),
  NewState = {Tag, Username, WriterPid, DifferentClients ++ Clients},
  {reply, ok, NewState};

%% Expected when this client is going to leave
%% Shutdowns writer and itself
handle_call(shutdown, _From, State) ->
  {_, _, WriterPid, Clients} = State,
  leave(Clients),
  WriterPid ! shutdown,
  {stop, normal, ok, State};

%% Expected when this client gets a new message
%% Saves message into database and print it
handle_call({print, Username, Message, PrivateKey, ChatId}, _From, State) ->
  {_, _, WriterPid, _} = State,
  DecryptedMessage = bobc_crypto:decrypt_message(Message, PrivateKey),
  save_message(ChatId, {Username, DecryptedMessage}),
  WriterPid ! {message, {Username, DecryptedMessage}},
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, [_Request | State], State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  io:format("Gen_server is off"),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encrypts and sends message for everyone in the Clients %%
%% if can't connect calls function found_dead_client      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_message({Name, Message, Clients}) ->
  F = fun(Client) ->
    {_, Address, Port, PublicKey} = Client,
    CryptoMessage = bobc_crypto:encrypt_message(Message, PublicKey),
    Msg = {message, Name, CryptoMessage},
    bobc_net:safe_send({Address, Port}, Msg, {?MODULE, found_dead_client, [Client, self()]})
      end,
  lists:map(F, Clients),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saves message into database %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_message(ChatId, {Name, Message}) ->
  Row = #message{name = Name, message = Message,
    msg_id = calendar:datetime_to_gregorian_seconds(calendar:local_time())},
  F = fun() -> mnesia:write(list_to_atom(ChatId),Row, write) end,
  mnesia:transaction(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sends every client a message that client is going to leave %%
%% if can't connect calls function found_dead_client          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
leave(Clients) ->
  [ThisClient | RemoteNodes] = lists:reverse(Clients),
  F = fun(Client) ->
    {_, Address, Port, _} = Client,
    Term = {left, ThisClient},
    bobc_net:safe_send({Address, Port}, Term, {?MODULE, found_dead_client, [Client, self()]})
      end,
  lists:map(F, RemoteNodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sends everyone information about current client   %%
%% if can't connect calls function found_dead_client %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
join(Clients, LastMsg) ->
  [ThisClient | RemoteNodes] = lists:reverse(Clients),
  F = fun(Client) ->
    {_, Address, Port, _} = Client,
    Term = {join, ThisClient, LastMsg},
    bobc_net:safe_send({Address, Port}, Term, {?MODULE, found_dead_client, [Client, self()]})
      end,
  lists:map(F, RemoteNodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sends everyone information about current client   %%
%% if can't connect calls function found_dead_client %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
join(Clients) ->
  [ThisClient | RemoteNodes] = lists:reverse(Clients),
  F = fun(Client) ->
    {_, Address, Port, _} = Client,
    Term = {join, ThisClient},
    bobc_net:safe_send({Address, Port}, Term, {?MODULE, found_dead_client, [Client, self()]})
      end,
  lists:map(F, RemoteNodes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sends a list of active Clients to the NewClient   %%
%% if can't connect calls function found_dead_client %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_clients_list(NewClient, Clients, Messages) ->
  {_, Address, Port, _} = NewClient,
  Term = {client_list, Clients, Messages},
  bobc_net:safe_send({Address, Port}, Term, {?MODULE, found_dead_client, [NewClient, self()]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tells gen_server that Client is offline %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
found_dead_client({Username, Host, Port, PublicKey}, ServerPid) ->
  gen_server:call(ServerPid, {left, {Username, Host, Port, PublicKey}}).