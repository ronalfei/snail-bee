%%because otp has module auth, so named this authd, means auth daemon.
%%
-module(authd).
-include("../snail.hrl").
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-export([index/2, index/3]).
-compile(export_all).

init({_Any, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
snail_action:run(?MODULE, Req, State).
%
%	try
%		snail_action:run(?MODULE, Req, State)
%	catch E1:E2 ->
%		lager:warning("~p catch a run warning < ~p ===== ~p >", [?MODULE, E1, E2]),
%		snail_action:reply(401, [{<<"www-authenticate">>, <<"501">>} ], [], Req, State)
%	end.

terminate(_Req, _State) ->
	ok.

%%-----------------------actions--------------------------
index(Req, State) ->
	index(Req, State, []).

index(Req, State, _Params) ->
	%----auth request's rid and token will be get from original url----
	{OriginalUri, Req} = cowboy_req:header(<<"X-Original-Uri">> ,Req,  ""),
	lager:debug("OriginalUri : ~p~n", [OriginalUri]), 
	{Path, QueryString, Fragment} = mochiweb_util:urlsplit_path(binary_to_list(OriginalUri)),
	lager:debug("Path : ~p, QueryString :~s, Fragment :~p", [Path, QueryString, Fragment]),
	Rid = get_rid(Path),

	EncryptedToken = token:get(Req, QueryString),
	lager:debug("rid=~p, t=~s", [Rid, EncryptedToken]),
	
	DecodeResult = token:decode(EncryptedToken, ""),
	case DecodeResult of
		{ok, Token} ->
			lager:debug("decode token ----- ~p", [Token]),
			case authorize(write, Rid, Token) of
			    {ok, true} ->
					snail_action:reply(200, [{<<"x-snail-status">>, Rid}, {<<"Access-Control-Allow-Method">>, <<"POST">>}], [], Req, State);
			    {error, Reason} ->
					snail_action:reply(401, [{<<"www-authenticate">>, Reason}], Reason, Req, State)
			end;
		{error, Reason} ->
			lager:debug("decode token error <~p> !", [Reason]),
			snail_action:reply(401, [{<<"www-authenticate">>, Reason}], [], Req, State)
	end.

%%---------------internal functions --------------------------
%%--but you must be export all--
get_rid(Path) ->
	PathList = string:tokens(Path, "/"),
	lager:debug("pathlist is ~p", [PathList]),
	Rid = lists:nth(3, PathList),
	Rid.

authorize(_Method, Rid, Token) ->
	FunList = [
		{?MODULE, check_rid, [_Method, Rid, Token]},
		{?MODULE, check_expiration, [Token]}
	],
	Result = util:andalso_apply(FunList),
	lager:info("authd fun list result is ~p", [Result]),
	Result.



check_rid(_Method, Rid, Token) ->
	Rid_ = util:binary_to_list(Rid),
	case Rid_ =:= Token#token.resource_id of
        true -> {ok, true};
        false -> {error, <<"token rid illegal">>}
    end.


check_expiration(Token) ->
	{T1, T2, _} = now(),
	CT = T1*1000000+T2,
	ET = list_to_integer(Token#token.expiration),
	lager:debug("~p===================~p", [CT, ET]),
	case ET  >  CT of
		true  -> {ok, true};
		false -> 
			lager:debug("token's expired!!!!!!!!!!!"),
			{error, <<"token expired">>}
	end.

