-module(logo).
-include("../snail.hrl").
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-compile(export_all).

init({_Any, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	snail_action:run(?MODULE, Req, State).

terminate(_Req, _State) ->
	ok.

%%-------------------actions-------------------------------

index(Req, State) ->
	index(Req, State, []).

index(Req, State, _Params) ->
	lager:debug("This is default action -> index"),
	snail_action:reply(200, [], <<"action index">>, Req, State).

%%--------------------upload-----------------------
upload(Req, State, _Params) ->
	lager:debug("req  ~p", [Req]),
	{[_Action, Rid | _Tail], Req}  = cowboy_req:path_info(Req),
	EncryptedToken = token:get(Req, ""),
    {ok, Token} = token:decode(EncryptedToken, ""),
	lager:debug("upload stream token ~p", [Token]),
	AuthRet = authd:authorize("upload stream", Rid, Token),
	lager:debug("auth result : ~p~n", [AuthRet]),
    case AuthRet of
        {error, Reason } ->
			snail_action:reply(401, [{<<"www-authenticate">>, Reason}], Reason, Req, State);
		{ok, true} ->
			FilePath = lists:flatten(resource_util:get_upload_path(Token#token.app_name, Rid)),
			case cowboy_req:body(Req) of 
				{ok, Data, Req1} -> 
					case fstream:append(FilePath, Data) of
						{ok, true} ->
							fstream:close(FilePath),
							ReturnToken = list_to_binary(EncryptedToken),
							snail_action:reply(200, [], <<"\"url\":\"/logo/view/", Rid/binary, "/?t=", ReturnToken/binary, "\"">>, Req1, State);	
						{error, Reason} ->
							snail_action:reply(506, [], [Reason], Req1, State)
					end;
				{error, Reason} ->
					snail_action:reply(501, [], Reason, Req, State);
				_Any	 ->
					lager:error("@@@@@@@@@@@@@@@@@@@@@logo body is @@@@@@@@@@@@@@@@@@~p ~n", [_Any]),
					snail_action:reply(501, [], _Any, Req, State)
			end
		end.

download(Req, State, _Params) ->
	lager:debug("This controller ~s, action -> download, Params is ~p ", [?MODULE, _Params]),
	{[_Action, Rid | _Tail], Req}  = cowboy_req:path_info(Req),
	EncryptedToken = token:get(Req, ""),
    {ok, Token} = token:decode(EncryptedToken, ""),
	lager:debug("download rid is ~p, token is ~p", [Rid, Token]),
	
	AuthRet = authd:authorize("download", Rid, Token),

	case AuthRet of
        {error, Reason } ->
            snail_action:reply(401, [{<<"www-authenticate">>, Reason}], Reason, Req, State);
        {ok, true} ->
			%% ps: headers include x-accl-redirct-url
			{Headers, Req1} = resource_util:generate_ngx_download_headers(Token, Req),
			snail_action:reply(200, Headers, [], Req1, State)
    end.


view(Req, State, _Params) ->
	lager:debug("This controller ~s, action -> download, Params is ~p ", [?MODULE, _Params]),
	{[_Action, Rid | _Tail], Req}  = cowboy_req:path_info(Req),
	EncryptedToken = token:get(Req, ""),
    {ok, Token} = token:decode(EncryptedToken, ""),
	lager:debug("download rid is ~p, token is ~p", [Rid, Token]),
	
	AuthRet = authd:authorize("download", Rid, Token),

	case AuthRet of
        {error, Reason } ->
            snail_action:reply(401, [{<<"www-authenticate">>, Reason}], Reason, Req, State);
        {ok, true} ->
			%% ps: headers include x-accl-redirct-url
			{Headers, Req1} = resource_util:generate_ngx_view_headers(Token, Req),
			snail_action:reply(200, Headers, [], Req1, State)
    end.



%-------------------internal function----------------



