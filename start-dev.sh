erl -sname snail-bee@localhost -pa ebin -pa deps/*/ebin -s snail -s reloader +K true -setcookie bee\
    -eval "io:format(\"~n~nThe following examples are available:~n\")." \
    -eval "io:format(\"* Hello world: http://localhost:8080~n\")." \
    -eval "io:format(\"* Websockets: http://localhost:8080/websocket~n\")." \
    -eval "io:format(\"* Eventsource: http://localhost:8080/eventsource~n~n\"). "
