-module(server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", socket_handler, []}
			   % {"/socket", socket_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}] ),
	io:format("----------------------~nSTARTED SERVER AT 8080~n--------------------~n"),
	server_sup:start_link().

stop(_State) ->
	ok.
