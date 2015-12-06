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

start() ->
	dets:open_file(t_players, [{keypos, #player.name}]),
	ets:new(t_players_online, [public, named_table, ordered_set]).

create_new_player(Name, Password) ->
	Player = #player{
		name = Name,
		pass = Password
	},
	dets:insert(t_players, Player),
	Player.


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

check_player_exists(Name) ->
	get_player(Name).

check_pass(Player, P) ->
	io:format(">> PLAYER RECORS: ~p~n", [Player]),
	Pass = Player#player.pass,
	if
		Pass == P ->
			ok;
		true ->
			not_ok	
	end.

set_player_online(Pid) ->
	ets:insert(t_players_online, {Pid}).

set_player_offline(Pid) ->
	ets:delete(t_players_online, {Pid}).
		


start_player(Name, SocketId) ->
	Pid = player:start(Name, SocketId),
	set_player_online(Pid),
	Pid.


broadcast_players_except_self(_Self, Msg) ->
	Pids = ets:tab2list(t_players_online),

	lists:foreach(
		fun
			({Pid}) when is_pid(Pid)->
				Pid ! Msg;
			(P) ->
				io:format("Other: ~p~n", P),
				ok
		end	, Pids),
	ok.

notify(newroom, Room, Self) ->
	broadcast_players_except_self(Self, {event_push_update_room, Room}).

