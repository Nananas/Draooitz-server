%% @doc Starts (only?) the login module...
%% @author Thomas Dendale

-module (server).

-export ([start/0]).
-export([init/0]).

start()->
	io:format("START~n"),
	Pid = spawn(?MODULE, init, []),
	{ok, Pid}.

init() ->
	io:format("INIT~n"),
	server_sup:start_child(login, start_survivor, []), 
	ok.



