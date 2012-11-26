%Last modified: 2012-05-07 23:11:20
%Author: BlackAnimal <ronalfei@qq.com>
%Create by vim: ts=4

-module(rabbitc).
-include("msgbus.hrl").

-compile(export_all).

open_connect(ConfigProplists) ->

	AmqpHost		= proplists:get_value("host", ConfigProplists),
	AmqpPort		= proplists:get_value("port", ConfigProplists),
	AmqpUsername	= proplists:get_value("username", ConfigProplists),
	AmqpPassword	= proplists:get_value("password", ConfigProplists),

	lager:info("~p, ~p, ~p, ~p, ~p", [ConfigProplists, AmqpHost, AmqpPort, AmqpUsername, AmqpPassword]),


	{ok, Connection} = amqp_connection:start(#amqp_params_network
		{
		  username	= list_to_binary(AmqpUsername)
		, password	= list_to_binary(AmqpPassword) 
		, host		= AmqpHost
		, port		= valida_integer(AmqpPort)
		, connection_timeout = 10
		}
	),
	lager:info("Connection Success: ~p", [Connection]),
	Connection.


open_channel(Connection) ->
	{ok, Channel} = amqp_connection:open_channel(Connection),
	lager:info("Open Channel Success: ~p", [Channel]),
	Channel.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recorde declare prototype
% #'queue.declare'{ticket = 0
% 	, queue = <<"">>, passive = false
% 	, durable = false, exclusive = false
% 	, auto_delete = false, nowait = false
% 	, arguments = []
% }).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_queue(Channel, QueueName) ->
	create_queue(Channel, QueueName, true).
create_queue(Channel, QueueName, Durable) ->
	QueueName_ = valida_binary(QueueName),

	#'queue.declare_ok'{queue = Queue}
		= amqp_channel:call(Channel
							,#'queue.declare'{queue=QueueName_, durable=Durable}),
	lager:info("Create a Queue Successed :~p", [Queue]),
	Queue.


push_message(Channel, QueueName, Message) ->
	QueueName_ = valida_binary(QueueName),
	Message_   = valida_binary(Message),
	Publish    = #'basic.publish'{ routing_key = QueueName_},
	Props	   = #'P_basic'{delivery_mode = 2}, %% persistent message
	Msg		   = #amqp_msg{props= Props, payload=Message_},
	amqp_channel:cast(Channel, Publish, Msg),
	lager:info("insert A message into :~p ,Message is : ~p", [QueueName_,Message_]),
	Message_.
	
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -record('basic.get'
%	, {ticket = 0
%		, queue = <<"">>
%, no_ack = false}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pull_message(Channel, QueueName) ->
	QueueName_ = valida_binary(QueueName),	
	Get        = #'basic.get'{queue=QueueName_},
	Return = case amqp_channel:call(Channel, Get) of
		{#'basic.get_ok'{delivery_tag=Tag}, Message} ->
			lager:debug("Get message ok, Tag: ~p, Message : ~p", [Tag, Message]),
			{Tag, Message};
		{'basic.get_empty', <<>>} ->
			lager:debug(" ~s is empty now, get nothing", [QueueName]),
			{false, empty}
	end,
	Return.

ack_message(Channel, Tag) ->
	Ack = #'basic.ack'{delivery_tag=Tag},
	amqp_channel:cast(Channel, Ack).

prefetch_message(Channel, PrefetchCount)->
	Qos = #'basic.qos'{prefetch_count=PrefetchCount},
	Ret = amqp_channel:call(Channel, Qos),
	lager:info("Set Qos Result : ~p", [Ret]).

close_channel(Channel) ->
	amqp_channel:close(Channel).

close_connect(Connection) ->
	amqp_connection:close(Connection).
	

valida_integer(In) ->
	if
		is_integer(In)	-> Out = In;
		is_list(In)		-> Out = list_to_integer(In);
		true			-> Out = In
	end,
	Out.

valida_binary(QueueName) ->
	if
        is_binary(QueueName) -> QueueName_ = QueueName;
        is_list(QueueName)   -> QueueName_ = list_to_binary(QueueName);
        true                 -> QueueName_ = list_to_binary(QueueName)
    end,
	QueueName_.

%%--------------------------------------------

%host("amqp")->
%    "127.0.0.1";
%
%host(_) ->
%    "127.0.0.1".
%
%
%port("amqp") ->
%    5672;
%port(_) ->
%    undefined.
%
%
%username("amqp") ->
%    "ronalfei";
%username(_) ->
%    "guest".
%
%
%password("amqp") ->
%    "batisfei";
%password(_) ->
%    "guest".
%
%




%test() ->
%	AmqpHost		= config:host("amqp"),
%	AmqpPort		= config:port("amqp"),
%	AmqpUsername	= config:username("amqp"),
%	AmqpPassword	= config:password("amqp"),
%
%	{H, M, S } = now(),
%    Time = integer_to_list(H)++integer_to_list(M)++integer_to_list(S),
%
%	ConfigProps = [
%		{"host", AmqpHost},
%		{"port", AmqpPort},
%		{"username", AmqpUsername},
%		{"password", AmqpPassword}
%	],
%
%    Connection = open_connect(ConfigProps),
%    Channel    = open_channel(Connection),
%	prefetch_message(Channel, 1),
%    Queue = create_queue(Channel, <<"test_queue_prefetch">>),
%    push_message(Channel, Queue, "this is a test prefetch content"++Time),
%
%    {Tag1, _Message1} = pull_message(Channel, Queue),
%    {Tag2, _Message2} = pull_message(Channel, Queue),
%    {Tag3, _Message3} = pull_message(Channel, Queue),
%
%
%    ack_message(Channel, Tag1),
%    ack_message(Channel, Tag2),
%    ack_message(Channel, Tag3),
%
%    close_channel(Channel),
%    close_connect(Connection),
%    ok.
%


