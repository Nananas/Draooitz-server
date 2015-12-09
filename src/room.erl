%% @author Thomas Dendale
%% @doc Room process and helper functions
-module (room).

-export ([start/1]).
-export ([loop/2]).
-export ([init/1]).

start(Name) ->
	{ok, Pid} = server_sup:start_child(room, init, [Name]), 
	Pid.


% @private
init(N) ->
	io:format("Starting a room: ~p ~n", [N]),
	T = ets:new(room_table, [public]),
	Pid = spawn(?MODULE, loop, [T,N]),
	{ok, Pid}.


% @private
loop(Table, Name) ->
	receive
		{add_player, PlayerId} ->
			io:format("inserting playerid: ~p, in ~p~n", [PlayerId, Table]),
			ets:insert(Table, {PlayerId}),
			loop(Table, Name);

		{remove_player, PlayerId} ->
			io:format("removing playerid: ~p~n", [PlayerId]),
			ets:delete(Table, PlayerId),
			loop(Table, Name);

		{notify, drawn_path, D, Me} ->
			ets:foldl(fun
				({Pid}, _Acc)  ->
					if
						Pid /= Me ->
							Pid ! {push_drawn_path, D},
							not_used;
						true -> 
							not_used
					end
			end, not_used, Table),
			loop(Table, Name);

		{notify, clear_drawing, Me} ->
			ets:foldl(fun
				({Pid}, _Acc)  ->
					if
						Pid /= Me ->
							Pid ! {clear_drawing},
							not_used;
						true -> 
							not_used
					end
			end, not_used, Table),
			loop(Table, Name);


		destroy ->
			io:format("destorying table"),
			ets:delete(Table),
			ok
	end.
