-module(ali_util).

-include("snail.hrl").
-compile(export_all).


send_task_to_mq(UploadPath, Type) ->
	ok.
		







%get_upload_path(AppName, Rid) ->
%	MfsDir = get_mfs_dir(Rid),
%	TailPath = get_tail_path(Rid),
%	FullPath = io_lib:format("~s~s/~s/upload/~s", [?UPLOAD_DATA_PREFIX, MfsDir, AppName, TailPath]),
%	filename:absname(FullPath).
%
%get_download_path(AppName, Rid) ->
%    MfsDir = get_mfs_dir(Rid),
%    TailPath = get_tail_path(Rid),
%    FullPath = io_lib:format("~s~s/~s/upload/~s", [?DOWNLOAD_DATA_PREFIX, MfsDir, AppName, TailPath]),
%	filename:absname(FullPath).
%
%
%
%%%-------------------------
%%% @doc Rid must be string or iolist
%get_tail_path(Rid) ->
%	RidStr = util:binary_to_list(Rid),
%	Dir1 = string:sub_string(RidStr, 11, 13),
%	Dir2 = string:sub_string(RidStr, 14, 16),
%	TailPath = io_lib:format("~s/~s/~s", [Dir1, Dir2, RidStr]),
%	TailPath.
