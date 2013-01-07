-module(mime).
-author("BlackAnimal <ronalfei@qq.com>").

-compile(export_all).

type(Type) when is_list(Type) ->
    Type1 = list_to_binary(Type),
    type_by_binary(Type1);
type(Type) when is_binary(Type) ->
    type_by_binary(Type).

type_by_binary(<<"html">>) ->  <<"text/html">>;

type_by_binary(<<"jpg">>) ->  <<"image/jpeg">>;
type_by_binary(<<"jpeg">>) ->  <<"image/jpeg">>;
type_by_binary(<<"gif">>) ->  <<"image/gif">>;
type_by_binary(<<"png">>) ->  <<"image/png">>;

type_by_binary(<<"mpeg">>) ->  <<"video/mpeg">>;
type_by_binary(<<"mpg">>) ->  <<"video/mpeg">>;
type_by_binary(<<"3gp">>) ->  <<"video/3gpp">>;
type_by_binary(<<"3gpp">>) ->  <<"video/3gpp">>;
type_by_binary(<<"mov">>) ->  <<"video/quicktime">>;
type_by_binary(<<"flv">>) ->  <<"video/x-flv">>;
type_by_binary(<<"mng">>) ->  <<"video/x-mng">>;
type_by_binary(<<"asf">>) ->  <<"video/x-ms-asf">>;
type_by_binary(<<"asx">>) ->  <<"video/x-ms-asf">>;
type_by_binary(<<"wmv">>) ->  <<"video/x-ms-wmv">>;
type_by_binary(<<"avi">>) ->  <<"video/x-msvideo">>;

%type_by_binary(<<"mp3">>) ->  <<"video/x-flv">>;
type_by_binary(<<"mp3">>) ->  <<"audio/mpeg">>;
type_by_binary(<<"ogg">>) ->  <<"audio/ogg">>;
type_by_binary(<<"ra">>) ->  <<"audio/x-realaudio">>;
type_by_binary(<<"mid">>) ->  <<"audio/midi">>;
type_by_binary(<<"midi">>) ->  <<"audio/midi">>;
type_by_binary(<<"kar">>) ->  <<"audio/midi">>;

type_by_binary(<<"doc">>) ->  <<"application/msword">>;
type_by_binary(<<"xls">>) ->  <<"application/vnd.ms-excel">>;
type_by_binary(<<"ppt">>) ->  <<"application/vnd.ms-powerpoint">>;
type_by_binary(<<"rtf">>) ->  <<"application/rtf">>;
type_by_binary(<<"pdf">>) ->  <<"application/pdf">>;
%    application/postscript                ps eps ai;



type_by_binary(_) -> <<"text/plain">>.

