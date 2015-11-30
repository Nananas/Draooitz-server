-module(server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export ([start_child/3]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		{?MODULE, {server, start, []}, permanent, brutal_kill, worker, [server]}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.

start_child(M, F, A) ->
	supervisor:start_child(?MODULE, {erlang:system_time(), {M, F, A}, 
		permanent, brutal_kill, worker, [M]}).
