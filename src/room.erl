-module (room).

-export ([start/1]).
-export ([loop/2]).
-export ([init/2]).

start(Name) ->
	T = ets:new(table, [public, ordered_set]),
	{ok, Pid} = server_sup:start_child(room, init, [T, Name]), 
	Pid.

init(T, N) ->
	io:format("Starting a room: ~p ~n", [N]),
	Pid = spawn(?MODULE, loop, [T,N]),
	{ok, Pid}.


loop(Table, Name) ->
	receive
		{add_player, PlayerId} ->
			ets:insert(Table, {PlayerId}),
			loop(Table, Name);

		{remove_player, PlayerId} ->
			ets:delete(Table, PlayerId),
			loop(Table, Name);

		{notify_drawn_path, D, Me} ->
			io:format("NOTIFY PATH~n"),
			ets:foldl(fun
				({Pid}, _Acc)  ->
					if
						Pid /= Me ->
							io:format("NICE"),
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
			ets:delete(Table),
			ok
	end.
