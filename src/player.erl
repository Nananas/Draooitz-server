%% @author Thomas Dendale
%% @doc Player process and helper functions.
-module (player).

-export ([start/2]).
-export ([init/2]).
-export ([loop/1]).
-export ([join_room/2]).
-export ([leave_room/2]).

-include ("records.hrl").

start(Name, SocketPid) ->
	{ok, Pid} = server_sup:start_child(?MODULE, init, [Name, SocketPid]),
	Pid.

join_room(Pid, Room) ->
	Pid ! {join_room, Room}.

leave_room(Pid, Room) ->
	Pid ! {leave_room, Room}.



% @private
init(Name, SocketPid) ->
	{ok, spawn(?MODULE, loop, [#{name=>Name, room=>none, socket=>SocketPid}])}.

% @private
loop(Args) ->
	receive
		{event_push_update_room, Room} ->
			#{socket := SocketPid} = Args,
			SocketPid ! {push_update_room, Room},
			loop(Args);

		{join_room, Room} ->
			NewArgs = Args#{room := Room},
			Pid = Room#room.pid,
			Pid ! {add_player, self()},
			loop(NewArgs);

		{leave_room} ->
			#{room := OldRoom} = Args,
			NewArgs = Args#{room := none},
			case OldRoom of
				none ->
					ok;
				_ ->
					Pid = OldRoom#room.pid,
					Pid ! {remove_player, self()}
			end,
			loop(NewArgs);

		{notify, drawn_path, D} ->
			#{room := Room} = Args,
			RoomId = Room#room.pid,
			RoomId ! {notify, drawn_path, D, self()},
			loop(Args);

		{notify, clear_drawing} ->
			#{room := Room} = Args,
			RoomId = Room#room.pid,
			RoomId ! {notify, clear_drawing, self()},
			loop(Args);


		{push_drawn_path, D} ->
			% io:format("PUSH~n"),
			#{socket := SocketPid} = Args,
			SocketPid ! {push_drawn_path, D},
			loop(Args);

		clear_drawing ->
			#{socket := SocketPid} = Args,
			SocketPid ! clear_drawing,
			loop(Args);


		destroy ->	
			players:set_player_offline(self()),
			#{room := Room} = Args,
			case Room of 
				none->
					ok;
				_ ->
					Rid = Room#room.pid,
					Rid ! {remove_player, self()}
			end,
			ok
	end.

