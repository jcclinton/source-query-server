-module(source_query_server_app).
-behavior(application).

-export([start/2, stop/1]).


start(normal, _Args) ->
	server_sup:start_link().

stop(_State) ->
	ok.
