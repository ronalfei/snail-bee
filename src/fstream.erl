-module(fstream).
-compile(export_all).

%%-include("snail.hrl").

append(Path, Bytes) ->
    try
        {ok, Out} = fstream_pool:curry(append, Path),
        {ok, true} = Out(Bytes)
    catch _:Exception ->
        lager:error("fstream append exception: ~p", [Exception]),
        {error, "failed to append  write file"}
    end.

pwrite(Path, Location, Bytes) ->
    try
        {ok, Out} = fstream_pool:curry(pwrite, Path),
        {ok, true} = Out(Location, Bytes)
    catch _:Exception ->
        lager:error("fstream pwrite exception: ~p", [Exception]),
        {error, "failed to write file"}
    end.

pread(Path, Location, Number) ->
    try
        {ok, In} = fstream_pool:curry(pread, Path),
        In(Location, Number)
    catch _:_ ->
        {error, "failed to read file"}
    end.

preadv(Path, LocNums) ->
    try
        {ok, In} = fstream_pool:curry(preadv, Path),
        In(LocNums)
    catch _:Exception ->
        lager:error("fstream pread exception: ~p", [Exception]),
        {error, "failed to read file"}
    end.

size(Path) ->
    try
        {ok, Info} = fstream_pool:curry(info, Path),
        {ok, Size} = Info(size),
        Size
    catch _:_ ->
        0
    end.

truncate(Path, Position) ->
	try
		{ok, TruncateFun} = fstream_pool:curry(truncate, Path),
		ok = TruncateFun(Position),
		{ok, Position+1}
	catch _A:_B ->
		ErrorReason = io_lib:format("truncate file ~s error, RA:~s, RB:~s", [Path, _A, _B]),
		lager:error("truncate file error :!!!!!!!!!!!!!!!!!!!!!!!!!!!!~s~n", [ErrorReason]),
		{error, ErrorReason}
	end.


close(Path) ->
	try
		fstream_pool:curry(close, Path)
	catch _A:_B ->
		ErrorReason = io_lib:format("close file ~p error, RA:~p, RB:~p", [Path, _A, _B]),
		lager:error("close file error :!!!!!!!!!!!!!!!!!!!!!!!!!!!!~s~n", [ErrorReason]),
		{error, ErrorReason}
	end.
		
