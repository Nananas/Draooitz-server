-module (room).

-export ([start/1]).
-export ([loop/2]).
-export ([init/1]).

start(Name) ->
	{ok, Pid} = server_sup:start_child(room, init, [Name]), 
	Pid.

init(N) ->
	io:format("Starting a room: ~p ~n", [N]),
	T = ets:new(room_table, [public]),
	Pid = spawn(?MODULE, loop, [T,N]),
	{ok, Pid}.


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

		{notify_drawn_path, D, Me} ->
			% io:format("NOTIFY PATH~n"),
			ets:foldl(fun
				({Pid}, _Acc)  ->
					if
						Pid /= Me ->
							% io:format("NICE"),
							Pid ! {push_drawn_path, D},
							not_used;
						true -> 
							not_used
					end
			end, not_used, Table),
			loop(Table, Name);
			% All = ets:match(Table, '$1'),

			% % io:format("ALL: match: ~p~n", [All]),
			% All_t = lists:map(fun (A)  ->
			% 	[{Name, _, _}] = A,
			% 	#room{name=Name},
			% 	Name = A#room.name,
			% 	[Name] = A,
			% 	#{name=>Name}		%% map = easier to convert to json
			% end, All);

		destroy ->
			io:format("destorying table"),
			ets:delete(Table),
			ok
	end.
