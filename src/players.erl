%% @author Thomas Dendale
%% @doc public functions for interaction with players and player processes
-module (players).

-export ([start/0]).
-export ([create_new_player/2]).
-export ([check_player_exists/1]).
-export ([check_pass/2]).
-export ([broadcast_players_except_self/2]).
-export ([get_player/1]).
-export ([notify/3]).
-export ([set_player_online/1]).
-export ([set_player_offline/1]).
-export ([start_player/2]).

-include ("records.hrl").


% @doc creates a new table, keeping track of online players
start() ->
	dets:open_file(t_players, [{keypos, #player.name}]),
	ets:new(t_players_online, [public, named_table, ordered_set]).

% @doc Creates a new player with specified Name and Password.
% @spec create_new_player(Name, Password) -> #player{}
%		Name = bitstring()
%		Password = bitstring()
create_new_player(Name, Password) ->
	Player = #player{
		name = Name,
		pass = Password
	},
	dets:insert(t_players, Player),
	Player.


% @doc Returns the player with specified Name, if found.
% @spec get_player(Name) -> {ok, #player{}} | not_found | error
%		Name = bitstring()
get_player(Name) ->
	Player = dets:match(t_players, #player{name=Name, pass='$1'}),

	case Player of 
		[[Pass]] ->
			{ok, #player{
				name=Name,
				pass=Pass
			}};

		[] ->
			not_found;

		{error, Reason} ->
			io:format("ERROR IN check_player_exists: ~p~n", [Reason]),
			error
	end.

% @doc Checks if a player with specified Name exists.
% @spec check_player_exists(Name) -> {ok, #player{}} | not_found | error
%		Name = bitstring()
check_player_exists(Name) ->
	get_player(Name).


% @doc Checks if a pass is correct for the specified Player.
% @spec check_pass (Player, Pass) -> ok | not_ok
%		Player = bitstring()
% 		Pass = bitstring()
check_pass(Player, P) ->
	Pass = Player#player.pass,
	if
		Pass == P ->
			ok;
		true ->
			not_ok	
	end.


% @doc Sets the specified Playerid _online_
% @spec set_player_online(pid()) -> ok
set_player_online(Pid) ->
	ets:insert(t_players_online, {Pid}).

% @doc Sets the specified Playerid _offline_
% @spec set_player_offline(pid()) -> ok
set_player_offline(Pid) ->
	ets:delete(t_players_online, Pid).
		

% @doc Starts a new player with specified Name and SocketId. This is a link with the
% websocket session and the player process.
% @spec start_player(Name, SocketId) -> pid()
%		Name = bitstring()
% 		SocketId = pid()
start_player(Name, SocketId) ->
	Pid = player:start(Name, SocketId),
	set_player_online(Pid),
	Pid.

% @doc Notifies every online player that a new room with specified Room name has been created.
% @spec notify(newroom, Room, Self) -> ok
% 		Room = bitstring()
% 		Self = pid()
notify(newroom, Room, Self) ->
	broadcast_players_except_self(Self, {event_push_update_room, Room}).

% @private
broadcast_players_except_self(_Self, Msg) ->
	Pids = ets:tab2list(t_players_online),

	lists:foreach(
		fun
			({Pid}) when is_pid(Pid)->
				Pid ! Msg;
			(_P) ->
				% io:format("Other: ~p~n", P),
				ok
		end	, Pids),
	ok.
