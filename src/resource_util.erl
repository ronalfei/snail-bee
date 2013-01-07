-module(resource_util).

-include("snail.hrl").
-compile(export_all).


get_upload_path(AppName, Rid) ->
	MfsDir = get_mfs_dir(Rid),
	TailPath = get_tail_path(Rid),
	FullPath = io_lib:format("~s~s/~s/upload/~s", [?UPLOAD_DATA_PREFIX, MfsDir, AppName, TailPath]),
	filename:absname(FullPath).

get_download_path(AppName, Rid) ->
    MfsDir = get_mfs_dir(Rid),
    TailPath = get_tail_path(Rid),
    FullPath = io_lib:format("~s~s/~s/upload/~s", [?DOWNLOAD_DATA_PREFIX, MfsDir, AppName, TailPath]),
	filename:absname(FullPath).

get_download_path(AppName, Rid, "preview") ->
	get_download_path(AppName, Rid) ++ ".preview";

get_download_path(AppName, Rid, "thumb") ->
	get_download_path(AppName, Rid) ++ ".thumb";

get_download_path(AppName, Rid, "") ->
	get_download_path(AppName, Rid).
	


%%-------------------------
%% @doc Rid must be string or iolist
get_tail_path(Rid) ->
	RidStr = util:binary_to_list(Rid),
	Dir1 = string:sub_string(RidStr, 11, 13),
	Dir2 = string:sub_string(RidStr, 14, 16),
	TailPath = io_lib:format("~s/~s/~s", [Dir1, Dir2, RidStr]),
	TailPath.
	


%%-------------------------
%% @doc Req is cowboy's req
%% @doc return {[Headers], Req2}
generate_ngx_download_headers(Token, Req) ->
	{AgentType, Req1} = cowboy_util:get_user_agent_type(Req),

	RawFileName = Token#token.resource_name,
    lager:debug("raw file name ~p", [RawFileName]),

    %DecodedFileName = case RawFileName =:= undefined of
    %    true -> Token#token.resource_id;
    %    false -> cowboy_http:urldecode(list_to_binary(RawFileName))
    %end,
	%原来是对文件名做了urldecode的.
	%现在的filename是通过token里解密出来的,所以刚好相反,是需要encode才对
	%同时,如果做了encode, 后面的re:replace应该可以删除了.待看
	DecodedFileName = case RawFileName =:= undefined of
        true -> Token#token.resource_id;
        false -> list_to_binary(RawFileName)
    end,
    ContentDisposition = case AgentType of
    	firefox ->
    	    FileName = re:replace(DecodedFileName, "\\+", "%20", [global, {return, list}]),
    	    lists:flatten( io_lib:format("attachment;filename*=utf8\'\'~s", [FileName]) );
    	msie ->
    	    FileName = re:replace(DecodedFileName, "\\+", "%20", [global, {return, list}]),
    	    lists:flatten( io_lib:format("attachment;filename=\"~s\"", [FileName]) );
    	undefined ->
    	    FileName = re:replace(DecodedFileName, "\\+", " ", [global, {return, list}]),
    	    lists:flatten( io_lib:format("attachment;filename=~s", [FileName]) )
    end,

	FilePath = resource_util:get_download_path(Token#token.app_name, Token#token.resource_id),
    Etag = token:get_etag_value(Token),
    NewQueryString = case Etag of
        [] -> "";
        _ ->  "?etag="++Etag
    end,
    Url = lists:flatten(io_lib:format("~s~s", [FilePath, NewQueryString])),
    lager:info("Last file url ..........~p ~n", [Url]),
    %let nginx determine the correct content type
    Headers = [
        {<<"X-Accel-Redirect">>, Url},
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"Content-Disposition">>, ContentDisposition}
    ],
	{Headers, Req1}.
	

%%-------------------------
%% @doc Req is cowboy's req
%% @doc return {[Headers], Req2}
%% @doc Option is "preview"|"thumb"|""
generate_ngx_view_headers(Token, Req, Option) ->
    FilePath = resource_util:get_download_path(Token#token.app_name, Token#token.resource_id, Option),
    Etag = token:get_etag_value(Token),
    NewQueryString = case Etag of
        [] -> "";
        _ ->  "?etag="++Etag
    end,
    Url = lists:flatten(io_lib:format("~s~s", [FilePath, NewQueryString])),
    lager:info("Last file url ..........~p ~n", [Url]),
    %let nginx determine the correct content type
	MimeType = list_to_binary(Token#token.resource_type),
    Headers = [
        {<<"X-Accel-Redirect">>, Url},
        {<<"Content-Type">>, mime:type(MimeType)}
    ],
    {Headers, Req}.



%------------------------------------
get_mfs_dir(_Rid) ->
	"".

%%根据rid来鉴定该文件是在哪个mfs中.
%%不过可能新方案还是通过增加节点来得方便.
%
%get_mfs_dir(Rid) when erlang:is_binary(Rid) ->
%	RidInt = erlang:list_to_integer(erlang:binary_to_list(Rid)),
%	if 
%		RidInt > 1400000000000000000 -> Dir = "01";
%		true -> Dir = "00"
%	end,
%	Dir;
%
%get_mfs_dir(Rid) when erlang:is_list(Rid) ->
%	RidInt = erlang:list_to_integer(Rid),
%	if 
%		RidInt > 1400000000000000000 -> Dir = "01";
%		true -> Dir = "00"
%	end,
%	Dir;
%
%get_mfs_dir(Rid) ->
%	if 
%		Rid > 1400000000000000000 -> Dir = "01";
%		true -> Dir = "00"
%	end,
%    Dir.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
