%%% -------------------------------------------------------------------
%%% Author  :   BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2012-6-11
%%% -------------------------------------------------------------------
-module(msgbus_pool).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("snail.hrl").

%% --------------------------------------------------------------------
%% External exports
%-export([start/0, start_link/0, stop/0, get_socket/0, free_socket/1, status/0]).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { config, connection, channel, queue, success, failed}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Config) ->
	WorkerName = list_to_atom(proplists:get_value("queue", Config)),
	lager:info("Workername is ~s ", [WorkerName]),
    gen_server:start_link({local, WorkerName}, ?MODULE, Config, []).

stop(Config) ->
	WorkerName = proplists:get_value("queue", Config),
    gen_server:call(WorkerName, stop).

%Msg is proplists
%push_oss will be defined in msgbus_pool.conf
put_message(WorkerName, Message) ->
	gen_server:call(WorkerName, {put, Message}).

%status(WorkerName) -> 
%	gen_server:call(WorkerName, status).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Config) ->
	{Connection, Channel, Queue} = connect_amqp_server(Config),
	lager:info("Init worker: ~p ,channel: ~p  successful self Pid:~p", [Queue, Channel, self()]),
    {ok, #state{config=Config, connection=Connection, channel=Channel, queue=Queue, success=0, failed=0}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)

%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(stop,_From, State) ->
	Channel		= State#state.channel,
	Connection	= State#state.connection,
	rabbitc:close_channel(Channel),
	rabbitc:close_connect(Connection),
	lager:info("worker is stoped"),
	{stop, shutdown,stoped, #state{} };

%Message type is proplists
handle_call({put, Message}, _From, State) ->
	Channel		= State#state.channel,
	QueueName	= State#state.queue,
	Msg = mochijson2:encode(Message),
	%lager:debug("json msg is ~p..............~n", [Msg]),
	rabbitc:push_message(Channel, QueueName, Msg),
	{ reply, {ok, Msg} , State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%%handle_cast({free_socket, Socket}, State) ->
%%	Sockets = State#state.sockets,
%%	Numbers = State#state.numbers,
%%	?dbg2("Free Socket:~p ", [Socket]),
%%	case Socket of 
%%		{error,_} ->
%%			{noreply, #state{ sockets=Sockets, numbers=Numbers } };
%%		_ ->
%%			inet:setopts(Socket, [{active, once}]),
%%			{noreply, #state{ sockets=[Socket|Sockets], numbers=Numbers+1 }} 
%%	end;


handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%%handle_info(kickoff, State) ->
%%	Channel	= State#state.channel,
%%	Queue	= State#state.queue,
%%	case rabbitc:pull_message(Channel, Queue) of
%%		{false, empty} ->
%%			erlang:send_after(2000, self(), kickoff),
%%			{noreply, State};
%%		{Tag, Message} ->
%%			do_job(State#state.config, Message, State#state.success),
%%			rabbitc:ack_message(Channel, Tag),
%%			self() ! kickoff,
%%			Successes = State#state.success +1 ,
%%			NewState = State#state{success = Successes},
%%			{noreply, NewState};
%%		What -> 
%%			lager:error("EEEEError: ~p", [What])
%%
%%	end;

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	lager:warning("Worker stoped at ~p, reason : ~p", [State, Reason]),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

connect_amqp_server(ConfigProplists) ->
	Connection	= rabbitc:open_connect(ConfigProplists),
	Channel		= rabbitc:open_channel(Connection),
	link(Channel),
	link(Connection),
	QueueName	= proplists:get_value("queue", ConfigProplists),
	rabbitc:prefetch_message(Channel, 1),
	Queue = rabbitc:create_queue(Channel, QueueName), 
	{Connection, Channel, Queue}.
	

%do_job(Config, Message, SuccessNumber) ->
%	Worker = list_to_atom(proplists:get_value("worker", Config)),
%	{amqp_msg, _MsgHeader, Msg} = Message,
%	Msg1 = binary_to_list(Msg),
%	Worker:run(Msg1, SuccessNumber).
