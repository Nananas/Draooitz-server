%% @author Thomas Dendale
% @doc public functions for interaction with rooms and room processes.

-module (rooms).

-export ([start/0]).
-export ([add_room/1]).
-export ([remove_room/1]).
-export ([remove_room/2]).
-export ([enterable/1]).
-export ([get_room/1]).
-export ([handle_create/1]).
-export ([handle_enter/1]).
-export ([handle_getlist/1]).

-include ("records.hrl").

% @doc creates a new table, keeping track of existing rooms
start() ->
	ets:new(t_rooms, [named_table, public, ordered_set, {keypos, #room.name}]).


% @doc Returns the existing room with the specified Name. Make sure the room exists!
% @spec get_room(Name) -> #room{}
% 		Name = bitstring()
get_room(Name) ->
	[R] = ets:lookup(t_rooms, Name),
	R.


% @doc Handles the incoming _create room_ message with a specified Name. 
% 		This will create a new room process if it does not yet exist, and add it to the tracking table.
% @spec handle_create(Name) -> {ok, #room{}} | not_ok
% 		Name = bitstring()
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


% @doc Handles the incoming _get room list_ message. Currently only the "ALL" message is supported. 
% @spec handle_getlist(Data) -> [#room{}]
% 		Data = bitstring()
handle_getlist(Data) ->
	case Data of
		<<"ALL">> ->
			All = ets:match(t_rooms, #room{name='$1', pid='$2', _='_'}),
			All_t = lists:map(fun (A)  ->
				[Name, Pid] = A,
				People = room:get_people_count(Pid),
				% io:format("people count = ~p~n", [People]),
				#{name=>Name, people=>People}		%% map = easier to convert to json
			end, All),
			% io:format("-> ~p ~n", [All_t]),
			All_t;
		_ ->
			[]
	end.


% @doc Handles the incoming _enter room_ message. Return if a room is enterable, and if so, also returns the room object.
% @spec handle_enter(Name) -> ok | not_ok
%		Name = bitstring()
handle_enter(Name) ->
	case enterable(Name) of
		ok ->
			{ok, get_room(Name)};
		not_ok ->
			not_ok
	end.



% @private
add_room(Room) ->
	ets:insert(t_rooms, Room).

% @private
% @spec remove_room (Room) -> ok.
% 		Room = #room{}
remove_room(Room) ->
	% {Name, Pid, _} = Room,
	Pid = Room#room.pid,
	Name = Room#room.name,
	remove_room(Pid, Name).

remove_room(Pid, Name) ->
	ets:delete(t_rooms, Name),
	Pid ! destroy.

% @private
enterable(Name) ->
	case ets:lookup(t_rooms, Name) of
		[_Object] ->
			ok;		% TODO: if too many people... make unable to enter
		[] ->
			not_ok;
		_ -> 
			not_ok
	end.


