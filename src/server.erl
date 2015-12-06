-module (server).

-export ([start/0]).
-export([init/0]).

start()->
	io:format("START~n"),
	Pid = spawn(?MODULE, init, []),
	% register(login_handler, Pid),
	% io:format("R: ~p ~n", [R]),
	{ok, Pid}.

init() ->
	io:format("INIT~n"),
	server_sup:start_child(login, start_survivor, []), 
	% server_sup:start_child(login, init_handler, []), 
	ok.



