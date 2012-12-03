-module(token).
-compile(export_all).
%-export([decode/2]).

-include("snail.hrl").
-define(XXTEA_KEY, "lenovodatalenovodata").

%%--------------easy to view
%%-record(token,
%%{
%%	  app_id				%% app_id equal to service_id
%%	, app_name				%% it's from app_id
%%    , option_type			%% upload or download
%%    , create_time			%% token's create time 
%%    , user_id
%%    , user_ip				%% client request ip
%%    , resource_id			%% rid
%%    , resource_type
%%    , resource_size
%%    , expiration			%% token expire time
%%    , etag					%% how to use it?
%%}).
%%---------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Req == cowboy req for get token from cookie
% @doc QueryString is from original url

get(Req, "") ->
	EncryptedToken = case cowboy_req:cookie(<<"TOKEN">>, Req, []) of
        {[], _Req} ->
			{T1, _Req2} = cowboy_req:qs_val(<<"t">>, Req, []),
            T1;
        {T2, _Req2} ->
            T2
    end,
    util:binary_to_list(EncryptedToken);

get(Req, QueryString) ->
	EncryptedToken = case cowboy_req:cookie(<<"TOKEN">>, Req, []) of
        {[], _Req} ->
            T1 = get_token(QueryString),
            T1;
        {T2, _Req} ->
            T2
    end,
	util:binary_to_list(EncryptedToken).


decode(EncryptedToken, Service) ->
	try
		{ok, DecryptedBin} = xxtea:decrypt(util:hex2bin(EncryptedToken), ?XXTEA_KEY),
		%lager:debug("~n++++++++++++++++++decryptedBin:~p++++++++++++++++++~n", [DecryptedBin]),
		Text = string:strip(binary_to_list(DecryptedBin), right, $\0),
		{ok, Token} = parse(Text, Service),
		lager:debug("~n++++++++++++++++++Token :~p++++++++++++++++++~n", [Token]),
		{ok, Token}
	catch E1:E2 ->
		lager:debug("eeeeeeeeeeeeeeeeeeeeeeeeeeeeedecode Token error ~p >>>> ~p", [E1, E2]),
		{error, "failed to decrypt token"}
	end.
	%%{ok, DecryptedBin} = xxtea:decrypt(util:hex2bin(EncryptedToken), ?XXTEA_KEY),
	%%lager:debug("~n++++++++++++++++++decryptedBin:~p++++++++++++++++++~n", [DecryptedBin]),
	%%Text = string:strip(binary_to_list(DecryptedBin), right, $\0),
	%%lager:debug("~n++++++++++++++++++Text:~p++++++++++++++++++~n", [Text]),
	%%{ok, Token} = parse(Text, Service),
	%%lager:debug("~n++++++++++++++++++Token :~p++++++++++++++++++~n", [Token]),
	%%{ok, Token}.




%%------------------------------------------------------------------------------------
parse(Text, _) ->
    parse(Text).

parse(Text) ->
    Props = text:tokenize(token, Text),

    Token = #token{ app_id = ?MODULE:get_props_field("APPID", Props, undefined)
            , option_type = ?MODULE:get_props_field("OT", Props, undefined)
            , create_time = ?MODULE:get_props_field("CT", Props, undefined)
            , user_id = ?MODULE:get_props_field("UID", Props, undefined)
            , user_ip = ?MODULE:get_props_field("UIP", Props, undefined)
            , resource_id = ?MODULE:get_props_field("RID", Props, undefined)
            , resource_type = ?MODULE:get_props_field("RT", Props, undefined)
            , resource_size = ?MODULE:get_props_field("RS", Props, undefined)
            , resource_name = ?MODULE:get_props_field("RN", Props, undefined)
            , expiration = ?MODULE:get_props_field("E", Props, undefined)
            , etag = ?MODULE:get_props_field("ET", Props, undefined)
    },
	AppName = ?MODULE:get_app_name(Token),
	Token1 = Token#token{app_name = AppName},
    {ok, Token1}.

get_props_field(Key, Props, DefaultVal) ->
    case proplists:lookup(Key, Props) of
        {Key, Val} -> Val;
        none -> DefaultVal
    end.

get_app_name(Token) ->
	case Token#token.app_id of
		"1"  -> "prf";
		"2"  -> "ent";
		"11" -> "bee";
		 _   -> ""
	end. 

get_etag_value(Token) ->
    ET = Token#token.etag,
    case ET of
        undefined -> "";
        _ -> ET
    end.

get_token(QueryString) ->
	QsProp = mochiweb_util:parse_qs(QueryString),
    lager:debug("result is ~p", [QsProp]),
    EncryptedToken = proplists:get_value("t", QsProp, []),
	EncryptedToken.
