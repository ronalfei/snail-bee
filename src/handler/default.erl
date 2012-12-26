-module(default).
-include("../snail.hrl").
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, _Opts) ->
	lager:debug("...........default handler accessed!~n ~p ~n", [Req]),
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [], <<"snail-bee is running!">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
