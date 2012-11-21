-module(resource).
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

upload(Req, State, _Params) ->
	lager:debug("~n~n ~p ~n~n", [Req]),

	lager:debug("This controller ~s, action -> upload, Params is ~p ", [?MODULE, _Params]),

	{[_Action, Rid| _Tail], _Req} = cowboy_req:path_info(Req),
	EncryptedToken = token:get(Req, ""),
	{ok, Token} = token:decode(EncryptedToken, ""),

	FilePath = lists:flatten(resource_util:get_upload_path(Token#token.app_name, Rid)),
	lager:debug("upload path : ~p ~n", [FilePath]),

	Form = cowboy_util:parse_form_new(Req),
	F = cowboy_util:parse_form_to_props(Form),
	
	TmpPath = util:binary_to_list(cowboy_util:get_res_tmp_path(F)),
	Md5		= cowboy_util:get_res_md5(F),
	lager:info("~n............................................................FilePath ~p ~n............................................................TmpPath  ~p ~n", [ FilePath, TmpPath]),
	NewReq = lists:last(Form),
	case file:rename(TmpPath, FilePath) of 
		ok ->
			Response = <<"{\"data\":{\"md5\":\"", Md5/binary, "\"},\"status\":true,\"code\":200,\"msg\":\"upload ok\"}">>,
			%Response = <<"uploadok">>,
			lager:info("upload Response: ~s ~n", [Response]),
			snail_action:reply(200, [{<<"Content-Type">>, <<"application/json; Charset=utf-8">>}], Response, NewReq, State);
		{error, Reason} ->
			Response = <<"{\"data\":{},\"status\":false,\"code\":500,\"msg\":\"upload error\"}">>,
			lager:error("upload rename Error: ~p", [Reason]),
			snail_action:reply(200, [{<<"www-authenticate">>, <<"502 transfer server error">>}], Response, NewReq, State)
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
	

%%------------uploadblock--------
%% @doc 
uploadblock(Req, State, _Params) ->
	{[_Action, Rid1 | _Tail], Req}  = cowboy_req:path_info(Req),
	%%客户端可能会包含文件后缀来请求，所以为了兼容， 需要去掉后缀名
	<<Rid:19/binary, _Rest/binary>> = Rid1,
	lager:debug("uploadblock Rid:~p++++++++++++++++++++++++++++~n", [Rid]),
	EncryptedToken = token:get(Req, ""),
	lager:debug("uploadblock Encrypted   token ~p+++++++++++++++++++~n", [EncryptedToken]),
    {ok, Token} = token:decode(EncryptedToken, ""),
	lager:debug("uploadblock token ~p", [Token]),
	AuthRet = authd:authorize("uploadblock", Rid, Token),
	{Cid, Req1} = cowboy_req:qs_val(<<"cid">>, Req),

	case AuthRet of 
		{error, Reason } ->
			%Response = <<"{\"data\":{},\"status\":false,\"code\":200,\"msg\":\"token error\"}">>,
			Response = <<"{\"result\":0,\"errno\":70401,\"errmsg\":\"token error\",\"cid\":\"",Cid/binary,"\",\"data\":{}}">>,
			snail_action:reply(200, [{<<"www-authenticate">>, Reason}], Response, Req1, State);
		{ok, true} ->
			{Start, End} = cowboy_util:get_range(Req),
			{ContentLengthBin, Req1} = cowboy_req:header('Content-Length', Req1),
			ContentLength = util:binary_to_integer(ContentLengthBin),
			RangeLength = End - Start +1,
			lager:debug("Start: ~p, End: ~p,Content-Length: ~p,RangeLength:~p", [Start, End, ContentLength, RangeLength]),
			FilePath = lists:flatten(resource_util:get_upload_path(Token#token.app_name, Rid)),
			lager:debug("upload block path : ~p ~n", [FilePath]),
			if
				ContentLength =/= RangeLength ->
					lager:debug("Error Range header"),
					Response = <<"{\"result\":0,\"errno\":70416,\"errmsg\":\"range error\",\"cid\":\"",Cid/binary,"\",\"data\":{}}">>,
					snail_action:reply(200, [{<<"www-authenticate">>, <<"405 range error">>}], Response, Req1, State);
				true ->
					%%todo: write content to file
					{ok, Body, Req2} = cowboy_req:body(Req1),
					lager:debug("upload block body is ~p", [Body]),
					case fstream:pwrite(FilePath, Start, Body) of
						{ok, _} ->
							lager:debug("write file ok"),
							Md5 = md5:string(Body),
							{T1, T2, _T3} = now(),
							Now = T1 * 1000000 + T2,
							Expire = Now + 600, % expire is 10 min
							lager:info("-------upload key  expire::::~p", [Expire]),
							Kdata = util:binary_to_list(Rid)++"|"++integer_to_list(Expire),
							lager:info("-------upload key  data ::::~p", [Kdata]),
							Key = util:bin2hex(xxtea:encrypt(Kdata, "beeuploadblockkey")),
							lager:info("-------upload block key ::::~p", [Key]),
							Response = io_lib:format(<<"{\"result\":1,\"errno\":70200,\"errmsg\":\"upload block ok\",\"cid\":~s,\"data\":{\"md5\":\"~s\",\"key\":\"~s\"}}">>, [Cid, Md5, Key]),
							snail_action:reply(200, [], Response, Req2, State);
						{error, Msg} ->
							Response = <<"{\"result\":0,\"errno\":70500,\"errmsg\":\"upload block error\",\"cid\":\"",Cid/binary,"\",\"data\":{}}">>,
							snail_action:reply(200, [{<<"X-Lenovo-Status">>, Msg}], Response, Req2, State)
					end
			end
	end.

%%------nginx 1.2 以上的proxy_pass支持http1.1的请求. 所以该方法可以直接交给nginx处理了
%%--但是如果客户端有代理服务器，那么这个头就会被抹掉。悲剧
%% @doc get_download_path to nginx, let nginx do block download self
downloadblock(Req, State, _Params) ->
	lager:debug("reqqqqq ~p", [Req]),
    {[_Action, Rid | _Tail], Req}  = cowboy_req:path_info(Req),

    EncryptedToken = token:get(Req, ""),
    {ok, Token} = token:decode(EncryptedToken, ""),
    lager:debug("downloadblock token ~p", [Token]),

    AuthRet = authd:authorize("downloadblock", Rid, Token),
    case AuthRet of
        {error, Reason } ->
            snail_action:reply(401, [{<<"www-authenticate">>, Reason}], Reason, Req, State);
        {ok, true} ->
			%% if Content-Range found in http header, so redirect download req to nginx
			case cowboy_req:header('Range', Req, undefined) of
				{ undefined, Req } ->
					{Start, End} = cowboy_util:get_range(Req),
					RangeLength = End - Start +1,
					%%open file will need absolutely path
					FilePath = lists:flatten(resource_util:get_upload_path(Token#token.app_name, Rid)),
					case fstream:pread(FilePath, Start, RangeLength) of
						{ok, Data} ->
            				snail_action:reply(200, [{<<"X-Lenovo-Result">>, <<"1">>}, {<<"X-Lenovo-Errno">>, <<"70200">>}], [Data], Req, State);
						{error, Reason} ->
            				snail_action:reply(200, [{<<"X-Lenovo-Result">>, <<"0">>}
													,{<<"X-Lenovo-Errno">>, <<"70500">>}
													,{<<"X-Lenovo-Msg">>, Reason}
													], [], Req, State);
						eof  ->
            				snail_action:reply(200, [{<<"X-Lenovo-Result">>, <<"0">>}
													,{<<"X-Lenovo-Errno">>, <<"70500">>}
													,{<<"X-Lenovo-Msg">>, <<"eof">>}
													], [], Req, State)
					end;
				_Any ->
					lager:debug("let nginx support range download"),
					{Headers, Req1} = resource_util:generate_ngx_download_headers(Token, Req),
					%%ps : var Headers will be include x-accel-redirect to nginx
            		%snail_action:reply(200, lists:append(Headers, [{<<"Content-Range">>, <<"bytes 0-12/-1">>}]), [], Req1, State)
					Headers1 = [ {<<"X-Lenovo-Result">>, <<"1">>}, {<<"X-Lenovo-Errno">>, <<"70200">>} | Headers],
            		snail_action:reply(200, Headers1, [], Req1, State)
			end
    end.

%%--------------------stream way-----------------------
uploadstream(Req, State, _Params) ->
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
			case cowboy_req:stream_body(Req) of 
				{ok, Data, Req1} -> 
					case fstream:append(FilePath, Data) of
						{ok, true} ->
							snail_action:reply(201, [], [], Req1, State);	
						{error, Reason} ->
							snail_action:reply(506, [], [Reason], Req1, State)
					end;
				{done, Req1} ->
					%close file handler
					fstream:close(FilePath),
					lager:info("....................................stream upload over: ~p", [FilePath]),
					snail_action:reply(200, [], [], Req1, State);
				{error, Reason} ->
					snail_action:reply(501, [], Reason, Req, State);
				_Any	 ->
					lager:error("@@@@@@@@@@@@@@@@@@@@@stream_body is @@@@@@@@@@@@@@@@@@~p ~n", [_Any]),
					snail_action:reply(501, [], _Any, Req, State)
			end
		end.


%%---------------------resource upload over/close--------------------
%%---------------------ensure file was completed-------------------
uploadclose(Req, State, _Params) ->
	{[_Action, Rid1 | _Tail], Req}  = cowboy_req:path_info(Req),
	%%客户端可能会包含文件后缀来请求，所以为了兼容， 需要去掉后缀名
	<<Rid:19/binary, _Rest/binary>> = Rid1,
	EncryptedToken = token:get(Req, ""),
    {ok, Token} = token:decode(EncryptedToken, ""),
	AuthRet = authd:authorize("uploadblock", Rid, Token),
	%--------------key 暂时先不做校验
	%{Cid, Req1} = cowboy_req:qs_val(<<"k">>, Req),
	%-----------------------------

	case AuthRet of 
        {error, Reason } ->
			Response = <<"{\"result\":0,\"errno\":70401,\"errmsg\":\"upload close error\",\"data\":{}}">>,
			snail_action:reply(200, [{<<"X-Lenovo-Status">>, 70401}, {<<"X-Lenovo-Msg">>, Reason}], Response, Req, State);
		{ok, true}       ->
			{SizeBin, Req1} = cowboy_req:qs_val(<<"s">>, Req),
			Size = util:binary_to_integer(SizeBin),
			FilePath = lists:flatten(resource_util:get_upload_path(Token#token.app_name, Rid)),
			FileSize = filelib:file_size(FilePath),
			lager:debug("Size is ~p, FileSize is ~p", [Size, FileSize]),
			case Size < FileSize of 
				true -> 
					fstream:truncate(FilePath, Size),
					Md5 = list_to_binary(util:get_file_md5_by_block(FilePath)),
					Response = <<"{\"result\":1,\"errno\":70200,\"md5\":", Md5/binary, ",\"errmsg\":\"upload close ok\"}">>,
					snail_action:reply(200, [], Response, Req1, State);
				_    ->
					Md5 = list_to_binary(util:get_file_md5_by_block(FilePath)),
					Response = <<"{\"result\":1,\"errno\":70202,\"md5\":", Md5/binary, ",\"errmsg\":\"upload close ok\"}">>,
					snail_action:reply(200, [], Response, Req1, State)
			end
	end.

%%-------------------internal function----------------



