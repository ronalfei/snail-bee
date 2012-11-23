-module(util).
-compile(export_all).

-include("snail.hrl").
-define(IN(X,Min,Max), X >= Min, X =< Max).

%%---------------------------------------------------------------------
%% @doc return result for andalso function
andalso_apply(FunList) ->
    andalso_apply([], FunList).
andalso_apply(Result, []) ->
    Result;
andalso_apply(_Result, [{M, F, A}| Tfun]) ->
    case erlang:apply(M, F, A) of
        {ok, true} -> andalso_apply({ok, true},  Tfun);
        {error, Reason} -> andalso_apply({error, Reason}, [])
    end.
	
%%---------------------------------------------------------------------
%% @doc return result for orelse function
orelse_apply(FunList) ->
    orelse_apply([], FunList).
orelse_apply(Result, []) ->
    Result;
orelse_apply(_Result, [{M, F, A}| Tfun]) ->
    case erlang:apply(M, F, A) of
        {ok, true} -> orelse_apply({ok, true},  []);
        {error, Reason} -> orelse_apply({error, Reason}, Tfun)
    end.

%%----------------------------------------------------------------------
%% @doc return the hex representation of a byte
byte_to_hex_string(X) ->
    [nibble_to_hex_char(X bsr 4),nibble_to_hex_char(X band 15)].

%%----------------------------------------------------------------------
%% @doc Convert an integer in 0..15 to a hex character

nibble_to_hex_char(X) when X < 10 -> $0 + X;
nibble_to_hex_char(X) -> $A + X - 10.

%%----------------------------------------------------------------------
%% @doc Convert a hex string to a binary.

hex2bin(Str) ->
    L = hex2list(Str),
    list_to_binary(L).
%%----------------------------------------------------------------------
%% @doc Convert a hex string to a list of bytes.

hex2list([H1,H2|T]) ->
    I = hex_nibble2int(H1) * 16 + hex_nibble2int(H2),
    [I|hex2list(T)];
hex2list([]) ->
    [].

%%----------------------------------------------------------------------
%% @doc Convert a hex nibble chanrcater to an integer.

hex_nibble2int(X) when ?IN(X, $0, $9) -> X - $0;
hex_nibble2int(X) when ?IN(X, $a, $f) -> X - $a + 10;
hex_nibble2int(X) when ?IN(X, $A, $F) -> X - $A + 10.

bin2hex(B) ->
    L = erlang:binary_to_list(B),
    LH0 = lists:map(fun(X)->erlang:integer_to_list(X,16) end, L),
    LH = lists:map(fun([X,Y])->[X,Y];([X])->[$0,X] end, LH0), % add zeros
    lists:flatten(LH).

uuid() ->
    T = term_to_binary({make_ref(), now()}),
    <<I:160/integer>> = crypto:sha(T),
    lists:flatten( io_lib:fwrite("~40..0s", [erlang:integer_to_list(I, 16)]) ).

%% url decode
url_decode([$%, Hi, Lo | Tail]) ->
    Hex = erlang:list_to_integer([Hi, Lo], 16),
    [Hex | url_decode(Tail)];
url_decode([$?|T]) ->
    %% Don't decode the query string here, that is parsed separately.
    [$?|T];
url_decode([H|T]) when is_integer(H) ->
    [H |url_decode(T)];
url_decode([]) ->
    [];
%% deep lists
url_decode([H|T]) when is_list(H) ->
    [url_decode(H) | url_decode(T)].

binary_to_list( Term ) when erlang:is_binary(Term) ->
	erlang:binary_to_list(Term);
binary_to_list( Term ) ->
	Term.

list_to_integer(Term) when erlang:is_list(Term) ->
	erlang:list_to_integer(Term);
list_to_integer(Term) ->
	Term.

binary_to_integer(Term) ->
	erlang:list_to_integer(erlang:binary_to_list(Term)).
	
get_file_md5_by_block(FilePath) ->
	Size = fstream:size(FilePath),
	case Size > 32 * 1024 *1024 of
		false -> 
			{ok, Data} = fstream:pread(FilePath, 0, Size),
			md5:string(Data);
		true  ->
			Blocks = 10,
			BlockSize = 64*1024,
			OffSet = Size div Blocks div BlockSize * BlockSize,
			%Context = erlang:md5_init(),
			Md5Fun = fun(X, Context) ->
				{ok, Data} = fstream:pread(FilePath, (X-1)*OffSet, BlockSize),
				lager:debug("...............offset is ~p ..........Context:~n ~p....................~n", [(X-1)*OffSet, Context]),
				erlang:md5_update(Context, Data)
			end,
			NewContext = lists:foldl(Md5Fun, erlang:md5_init(), lists:seq(1,10)),
			lager:debug("~n~n.......................newnewnewnewn~p................~n~n", [NewContext]),
			LastOffSet = Size - BlockSize,
			{ok, LastData} = fstream:pread(FilePath, LastOffSet, BlockSize),
			LastContext = erlang:md5_update(NewContext, LastData),
			Digest = erlang:md5_final(LastContext),
			Md5 = string:to_lower(bin2hex(Digest)),
			lager:info("file block md5 is ~p", [Md5]),
			Md5
	end.




