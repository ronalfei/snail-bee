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
type_by_binary(_) -> <<"text/plain">>.

