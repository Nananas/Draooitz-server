-module (room).

-export ([start/1]).
-export ([loop/2]).
-export ([init/2]).

start(Name) ->
	T = ets:new(table, [set]),
	{ok, Pid} = server_sup:start_child(room, init, [T, Name]), 
	Pid.

init(T, N) ->
	io:format("Starting a room: ~p ~n", [N]),
	Pid = spawn(?MODULE, loop, [T,N]),
	{ok, Pid}.


loop(Table, Name) ->
	receive
		{add_player, Player} ->
			ets:insert(Table, Player),
			loop(Table, Name);

		{remove_player, Player} ->
			#{user := User} = Player,
			ets:delete(Table, User),
			loop(Table, Name);

		destroy ->
			ets:delete(Table),
			ok
	end.
