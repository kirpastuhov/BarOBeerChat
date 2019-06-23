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
start_link({Server, WriterPid}) ->
  gen_server:start_link(?MODULE, [Server, WriterPid], []).

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
init([Server, WriterPid]) ->
  Host = "localhost",
  {ok, #state{name = Server, writer = WriterPid, clients = [{Host, 4200}, {Host, 1234}]}}.

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


handle_call(shutdown, _From, State) ->
  {_, _, WriterPid, _} = State,
  WriterPid ! shutdown,
  {stop, normal, ok, State};


handle_call({print, Username, Message}, _From, State) ->
  {_, _, WriterPid, _} = State,
  %% TODO create fun that saves messages
  save_message({Username, Message}),
  WriterPid ! {message, {Username, Message}},
  {reply, ok, State};




handle_call(_Request, _From, State) ->
  {reply, [_Request|State], State}.

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
    {ok, Socket} =  gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    Msg = {Name, Message},
    gen_tcp:send(Socket, term_to_binary(Msg))
    end,
    lists:map(F, Clients),
    ok.





save_message({Name, Message}) ->

  Row = #message{name = Name, message = Message,
    msg_id = mnesia:dirty_last(message) + 1},
  F = fun () -> mnesia:write(Row) end,
  mnesia:transaction(F).