%%%-------------------------------------------------------------------
%%% @author User
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июнь 2019 15:57
%%%-------------------------------------------------------------------
-module(chat_server).
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
  code_change/3]).

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
start_link({Username, WriterPid, Clients}) ->
  gen_server:start_link(?MODULE, [Username, WriterPid, Clients], []).

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
init([Username, WriterPid, Clients]) ->
  if
    length(Clients) /= 1 ->
      join(Username, Clients);
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

handle_call({send, Message}, _From, State) ->
  {_, Name, _, _} = State,
  send_message({Name, Message, State}),
  {reply, ok, State};

handle_call({joined, Client}, _From, State) ->
  {Tag, Username, WriterPid, Clients} = State,
  {Guest, Address, Port} = Client,
  WriterPid ! {message, {Guest, "joined"}},
  send_clients_list({Address, Port}, Clients),
  NewState = {Tag, Username, WriterPid, [{Address, Port} | Clients]},
  {reply, ok, NewState};

handle_call({left, Client}, _From, State) ->
  {Tag, Username, WriterPid, Clients} = State,
  {Guest, Address, Port} = Client,
  WriterPid ! {message, {Guest, "left"}},
  NewClients = lists:delete({Address, Port}, Clients),
  NewState = {Tag, Username, WriterPid, NewClients},
  {reply, ok, NewState};

handle_call({join, NewClients}, _From, State) ->
  {Tag, Username, WriterPid, Clients} = State,
  [ThisClient | RemoteNodes ] = lists:reverse(Clients),
  DifferentClients = get_difference(RemoteNodes, NewClients),
  join(Username, lists:reverse([ThisClient | DifferentClients])),
  NewState = {Tag, Username, WriterPid, DifferentClients ++ Clients},
  {reply, ok, NewState};


handle_call(shutdown, _From, State) ->
  {_, Username, WriterPid, Clients} = State,
  leave(Username, Clients),
  WriterPid ! shutdown,
  {stop, normal, ok, State};


handle_call({print, Username, Message}, _From, State) ->
  {_, _, WriterPid, _} = State,
  %% TODO create fun that saves messages
  save_message({Username, Message}),
  WriterPid ! {message, {Username, Message}},
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


send_message({Name, Message, State}) ->
  Clients = State#state.clients,
  F = fun(Client) ->
    {Host, Port} = Client,
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    Msg = {message, Name, Message},
    gen_tcp:send(Socket, term_to_binary(Msg))
      end,
  lists:map(F, Clients),
  ok.



save_message({Name, Message}) ->

  Row = #message{name = Name, message = Message,
    msg_id = mnesia:dirty_last(message) + 1},
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F).



leave(Username, Clients) ->
  [ThisClient | RemoteNodes] = lists:reverse(Clients),
  {LocalAddress, LocalPort} = ThisClient,
  F = fun(Client) ->
    {Address, RemotePort} = Client,
    {ok, Socket} = gen_tcp:connect(Address, RemotePort, [binary, {active, true}, {packet, raw}]),
    gen_tcp:send(Socket, term_to_binary({left, {Username, LocalAddress, LocalPort}})),
    gen_tcp:close(Socket)
      end,

  lists:map(F, RemoteNodes).

join(Username, Clients) ->
  [ThisClient | RemoteNodes] = lists:reverse(Clients),
  {LocalAddress, LocalPort} = ThisClient,

  F = fun(Client) ->
    {Address, RemotePort} = Client,
    {ok, Socket} = gen_tcp:connect(Address, RemotePort, [binary, {active, true}, {packet, raw}]),
    gen_tcp:send(Socket, term_to_binary({join, {Username, LocalAddress, LocalPort}})),
    gen_tcp:close(Socket)
      end,

  lists:map(F, RemoteNodes).

send_clients_list(NewClient, Clients) ->
  {Address, Port} = NewClient,
  {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {active, true}, {packet, raw}]),
  gen_tcp:send(Socket, term_to_binary({client_list, Clients})),
  gen_tcp:close(Socket).

get_difference(List1, List2) ->
  F = fun(Client, DifferenceList) ->
    Val = lists:member(Client, List2),
    if Val -> DifferenceList;
      true -> [Client | DifferenceList]
    end
      end,
  F2 = fun(Client, DifferenceList) ->
    Val = lists:member(Client, List1),
    if Val -> DifferenceList;
      true -> [Client | DifferenceList]
    end
       end,
  lists:foldl(F2, [], List2) ++ lists:foldl(F, [], List1).