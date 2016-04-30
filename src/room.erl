%% @author Thomas Dendale
%% @doc Room process and helper functions
-module (room).

-export ([start/1]).
-export ([loop/2]).
-export ([init/1]).
-export ([get_people_count/1]).

start(Name) ->
	{ok, Pid} = server_sup:start_child(room, init, [Name]), 
	Pid.


% @private
init(N) ->
	io:format("Starting a room: ~p ~n", [N]),
	Pid = spawn(?MODULE, loop, [[],N]),
	{ok, Pid}.


% @private
loop(Players, Name) ->
	receive
		% the player messages this.
		{add_player, PlayerId} ->
			NewPlayers = [PlayerId] ++ Players,
			players:notify(update_room, Name, length(NewPlayers), PlayerId),
			loop(NewPlayers, Name);

		% the player messages this.
		{remove_player, PlayerId} ->
			NewPlayers = lists:delete(PlayerId, Players),
			players:notify(update_room, Name, length(NewPlayers), PlayerId),
			loop(NewPlayers, Name);

		{notify, draw_path, D, Me} ->
			notify_other_players(Players, {push_draw_path, D}, Me),
			loop(Players, Name);

		{notify, clear_drawing, Me} ->
			notify_other_players(Players, {clear_drawing}, Me),
			loop(Players, Name);

		{rpc, get_people_count, Sender} ->
			Sender ! {self(), length(Players)},
			loop(Players, Name);

		{get_players, Sender} ->
			Sender ! {Players},
			loop(Players, Name);


		destroy ->
			ok

		after
			1000*60*10 ->
				Size = length(Players),

				if 
					Size == 0 ->
						rooms:remove_room(self(), Name),
						% loop(Table, Name);
						loop(Players, Name);
					true ->
						% loop(Table, Name)
						loop(Players, Name)
				end
	end.

% @doc sends a message to all other players, except Self, currently in the room
% @private
notify_other_players(List, Message, Self) ->
	lists:foreach(fun
		(Pid) ->
			if
				Pid /= Self ->
					Pid ! Message;
				true ->
					ok	
			end
	end, List).

% @doc Sends a message to room with process id Pid, requesting the amount of players in the room.
% @spec get_people_count(pid()) -> int()
get_people_count(Pid) ->
	rpc(Pid, get_people_count).

% @private
% call which waits for a return message
rpc(Pid, Request) ->
	Pid ! {rpc, Request, self()},
	receive
		{Pid, Response} -> 
			Response
	end.
