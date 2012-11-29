%This is an  action controller
-module(snail_action).
-include("snail.hrl").
%-compile(export_all).
-export([run/3, reply/5]).

%run(Module, Req, State) ->
%	{Path_info, Req} = cowboy_req:path_info(Req), 
%	case Path_info of
%		[] -> erlang:apply(Module, index, [Req, State]);
%		[ActionBin | TailParam]  -> 
%			Action = erlang:binary_to_atom(ActionBin, latin1),
%			erlang:apply(Module, Action, [Req, State, TailParam]);
%		_Any -> 
%			lager:debug("path info any -----------~p", [_Any]),
%			not_found(Module, Req, State)
%	end.


run(Module, Req, State) ->
	lager:info("~n~nrequest begin............................."),
	{Path_info, Req} = cowboy_req:path_info(Req), 
	lager:debug("~nRREEQQ:::~n~n ~p", [Req]),
	case Path_info of
		[] -> erlang:apply(Module, index, [Req, State]);
		[ActionBin | TailParam]  -> 
			Action = erlang:binary_to_atom(ActionBin, latin1),
			try	erlang:apply(Module, Action, [Req, State, TailParam])
			catch E1:E2 ->
				case E2 of
					undef -> not_found(Module, Req, State);
					_	  -> server_error(Module, Req, State, {E1,E2})
				end
			end;
		_Any -> 
			lager:debug("path info any -----------~p", [_Any]),
			not_found(Module, Req, State)
	end.

not_found(Module, Req, State) ->
	lager:debug("This controller ~s action -> not found", [Module]),
	Response = <<"{\"data\":[],\"status\":false,\"code\":404,\"msg\":\"client error not found\"}">>,
	?MODULE:reply(404, [], Response, Req, State).

server_error(Module, Req, State, Error) ->
	lager:error("Server error in module: ~s, Reason: ~p ", [Module, Error]),
	Response = <<"{\"data\":[],\"status\":false,\"code\":500,\"msg\":\"server error not found\"}">>,
	?MODULE:reply(500, [], Response, Req, State).

reply(Status, Headers, Body, Req, State) ->
	{Path_info, Req} = cowboy_req:path_info(Req),
	lager:info("Path info << ~p >> is replyed, status: << ~p >>", [Path_info, Status]),
	lager:info("~nrequest   end.............................~n"),
	{ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req),
    {ok, Req2, State}.

