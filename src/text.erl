-module(text).
-export([tokenize/2]).

tokenize(qs, Text) ->
    tokenize(Text, "&", "=");

tokenize(token, Text) ->
    tokenize(Text, ";", ":").


tokenize(Text, S1, S2) ->
   Fun = fun(Elem, AccIn) ->
        case string:tokens(Elem, S2) of
            [Key, Val] ->
                [{Key, Val}|AccIn];
            [Key] ->
                [{Key, ""}|AccIn];
            [] ->
                AccIn
        end
    end,

    lists:foldr(Fun, [], string:tokens(Text, S1)).
