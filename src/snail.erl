-module(snail).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-export([log/1]).

start() ->
	application:start(crypto),
%	application:start(public_key),
%	application:start(ssl),
	application:start(ranch),
	application:start(cowboy),
	lager:start(),
	application:start(snail).

start(_Type, _Args) ->
%%%you can config hosts here ,because cowboy support vhost
	Dispatch = [
		{	[<<"tp">>,<<"lenovows">>,<<"com">>],
			[
				{[<<"resource">>, '...'],	resource,	[]},
				{[<<"file">>, '...'],		resource,	[]},
				{[<<"auth">>, '...'],		authd,		[]},
                {[<<"logo">>, '...'],       logo,       []},
				{'_', default, []}
			]
		},
		{	[<<"tp">>,<<"hivews">>,<<"com">>],
			[
				{[<<"resource">>, '...'],	resource,	[]},
				{[<<"file">>, '...'],		resource,	[]},
				{[<<"auth">>, '...'],		authd,		[]},
                {[<<"logo">>, '...'],       logo,       []},
				{'_', default, []}
			]
		},


		{'_', [
            {'_', default, [{error, 404}]}
        ]}
	],
	cowboy:start_http(snail_http_listener, 10, [{port, 8080}], [{dispatch, Dispatch}]),
%	cowboy:start_listener(snail_http_listener, 10,
%		cowboy_tcp_transport, [{port, 8080}],
%		cowboy_http_protocol, [{dispatch, Dispatch}]
%	),


%%%%we backup some example here
%	Dispatch = [
%		{'tp.lenovobee.com', [
%			{[<<"websocket">>], websocket_handler, []},
%			{[<<"eventsource">>], eventsource_handler, []},
%			{[<<"eventsource">>, <<"live">>], eventsource_emitter, []},
%			{'_', default_handler, []}
%		]}
%	],
%	cowboy:start_listener(my_http_listener, 100,
%		cowboy_tcp_transport, [{port, 8080}],
%		cowboy_http_protocol, [{dispatch, Dispatch}]
%	),

%%%%we are not need Https 
%	cowboy:start_listener(my_https_listener, 100,
%		cowboy_ssl_transport, [
%			{port, 8443}, {certfile, "priv/ssl/cert.pem"},
%			{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
%		cowboy_http_protocol, [{dispatch, Dispatch}]
%	),
%%%%-------------------------
	application:start(inets),
	snail_sup:start_link(),
	msgbus_pool_sup:start_link().

stop(_State) ->
	ok.




%%----------------------
log(debug) ->
    lager:set_loglevel(lager_console_backend, debug);
log(info) ->
    lager:set_loglevel(lager_console_backend, info);
log(warning) ->
    lager:set_loglevel(lager_console_backend, warning);
log(error) ->
    lager:set_loglevel(lager_console_backend, error).
