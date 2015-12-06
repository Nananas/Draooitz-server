-module (player).

-export ([start/2]).
-export ([init/2]).
-export ([loop/1]).
-export ([join_room/2]).
-export ([leave_room/2]).

start(Name, SocketPid) ->
	{ok, Pid} = server_sup:start_child(?MODULE, init, [Name, SocketPid]),
	Pid.

init(Name, SocketPid) ->
	{ok, spawn(?MODULE, loop, [#{name=>Name, room=>none, socket=>SocketPid}])}.

loop(Args) ->
	receive
		{event_push_update_room, Room} ->
			#{socket := SocketPid} = Args,
			SocketPid ! {push_update_room, Room},
			loop(Args);

		{join_room, Room} ->
			NewArgs = Args#{room := Room},
			loop(NewArgs);

		{leave_room} ->
			NewArgs = Args#{room := none},
			loop(NewArgs);

		destroy ->	
			players:set_player_offline(self()),
			ok
	end.

join_room(Pid, Room) ->
	Pid ! {join_room, Room}.

leave_room(Pid, Room) ->
	Pid ! {leave_room, Room}.