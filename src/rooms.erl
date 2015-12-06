-module (rooms).

-export ([start/0]).
-export ([add_room/1]).
-export ([remove_room/1]).
-export ([enterable/1]).
-export ([get_room/1]).
-export ([handle_create/1]).
-export ([handle_enter/1]).
-export ([handle_getlist/1]).

-include ("records.hrl").

start() ->
	ets:new(t_rooms, [named_table, public, ordered_set, {keypos, #room.name}]).


%% PRIVATE
add_room(Room) ->
	ets:insert(t_rooms, Room).

%% PRIVATE
remove_room(Room) ->
	% {Name, Pid, _} = Room,
	Pid = Room#room.pid,
	Name = Room#room.name,
	ets:delete(t_rooms, Name),
	Pid ! destroy.
	
%% PRIVATE
enterable(Name) ->
	case ets:lookup(t_rooms, Name) of
		[_Object] ->
			ok;		% TODO: if too many people... make unable to enter
		[] ->
			not_ok;
		_ -> 
			not_ok
	end.


% -----------
%% PUBLIC
% -----------

get_room(Name) ->
	[R] = ets:lookup(t_rooms, Name),
	R.



handle_create(Name) ->
	case ets:lookup(t_rooms, Name) of
		[_Object] ->
			not_ok;	%% already exists
		[] ->
			RoomPid = room:start(Name),
			Room = #room{name = Name, pid = RoomPid},
			add_room(Room),
			{ok, Room}
	end.


handle_getlist(Data) ->
	case Data of
		<<"ALL">> ->
			All = ets:match(t_rooms, #room{name='$1', _='_'}),
			io:format("ALL: match: ~p~n", [All]),
			All_t = lists:map(fun (A)  ->
				% [{Name, _, _}] = A,
				% #room{name=Name},
				% Name = A#room.name,
				#{name=>A}		%% map = easier to convert to json
			end, All),
			io:format("-> ~p ~n", [All_t]),
			All_t;
		_ ->
			[]
	end.


handle_enter(Name) ->
	case enterable(Name) of
		ok ->
			{ok, get_room(Name)};
		not_ok ->
			not_ok
	end.
