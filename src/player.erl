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
		%
		{event_push_new_room, Room} ->
			#{socket := SocketPid} = Args,
			SocketPid ! {push_new_room, Room},
			loop(Args);

		{event_push_update_room, Room, PlayerCount} ->
			#{socket := SocketPid} = Args,
			SocketPid ! {push_update_room, Room, PlayerCount},
			loop(Args);

		% joining room by updating variable, and sending a message the Roomid to add this player to its list
		{join_room, Room} ->
			NewArgs = Args#{room := Room},
			Rid = Room#room.pid,
			Rid ! {add_player, self()},
			loop(NewArgs);

		% check if in a room, and send a message to that Roomid if true
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


		% notify loopback for DRAWPATH message
		{notify, draw_path, D} ->
			#{room := Room} = Args,
			RoomId = Room#room.pid,
			RoomId ! {notify, draw_path, D, self()},
			loop(Args);

		{push_draw_path, D} ->
			#{socket := SocketPid} = Args,
			SocketPid ! {push_drawn_path, D},
			loop(Args);

		% notify loopback for CLEARDRAWING message
		{notify, clear_drawing} ->
			#{room := Room} = Args,
			RoomId = Room#room.pid,
			RoomId ! {notify, clear_drawing, self()},
			loop(Args);

		{clear_drawing} ->
			#{socket := SocketPid} = Args,
			SocketPid ! clear_drawing,
			loop(Args);

		%
		{update_player_count_of_room, RoomName, PlayerCount} ->
			#{socket := SocketPid} = Args,
			SocketPid ! {push_update_room, RoomName, PlayerCount},
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

