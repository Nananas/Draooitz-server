-module(socket_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include ("records.hrl").

init(_, _, _) ->
	io:format("-------------- INIT~n"),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	Req2 = cowboy_req:compact(Req),
	% io:format("> WS INIT~n"),
	{ok, Req2, not_logged_in}.



% State -> Player pid if logged in
websocket_handle({text, Data}, Req, not_logged_in) ->
	case Data of

		<<"LOGIN:", D/bitstring>> ->
			io:format("LOGIN: "),
			Result = login:handle_login(D),
			case Result of
				{ok, Player} ->
					io:format("ok~n"),
					Pid = players:start_player(Player#player.name, self()),
					{reply, {text, tojson(#{state => <<"LOGGED_IN">>})}, Req, Pid};

				not_ok ->
					io:format("not_ok~n"),
					{reply, {text, tojson(#{state => <<"NOT_LOGGED_IN">>})}, Req, not_logged_in}
			end;

		_->
			{reply, {text, tojson(#{state => <<"NOT_LOGGED_IN">>})}, Req, not_logged_in}
	end;

	
websocket_handle({text, Data}, Req, PlayerPid) when is_pid(PlayerPid) ->
	case Data of
		%% LOGIN as someone else
		<<"LOGIN:", D/bitstring>> ->
			PlayerPid ! destroy,
			io:format("LOGIN: "),
			Result = login:handle_login(D),
			case Result of
				{ok, Player} ->
					io:format("ok~n"),
					Pid = players:start_player(Player#player.name, self()),
					{reply, {text, tojson(#{state => <<"LOGGED_IN">>})}, Req, Pid};

				not_ok ->
					io:format("not_ok~n"),
					{reply, {text, tojson(#{state => <<"NOT_LOGGED_IN">>})}, Req, not_logged_in}
			end;

		<<"CREATE:", N/bitstring>> ->
			Msg = case rooms:handle_create(N) of
				not_ok ->
					not_ok;
				{ok, R} ->
					player:join_room(PlayerPid, R),
					ok
			end,

			{reply, {text, tojson(Msg)}, Req, PlayerPid};

		<<"GETROOMLIST:", D/bitstring>> ->
			R = rooms:handle_getlist(D),
			% io:format("GETROOMLIST: ~p ~n",  [R]),
			{reply, {text, tojson(R)}, Req, PlayerPid};

		<<"NEWROOM:", N/bitstring>> ->
			case rooms:handle_create(N) of
				{ok, Room} ->
					PlayerPid ! {join_room, Room},
					players:notify(newroom, Room, PlayerPid),
					{reply, {text, tojson(ok)}, Req, PlayerPid};
				not_ok ->
					{reply, {text, tojson(not_ok)}, Req, PlayerPid}
			end;

		<<"ENTERROOM:", N/bitstring>> ->
			case rooms:handle_enter(N) of
				{ok, Room} ->
					PlayerPid ! {join_room, Room},
					{reply, {text, tojson(ok)}, Req, PlayerPid};
				not_ok ->
					{reply, {text, tojson(not_ok)}, Req, PlayerPid}
			end;

		<<"LEAVEROOM">>	->
			PlayerPid ! {leave_room},
			{reply, {text, tojson(ok)}, Req, PlayerPid};

		%% TODO: parse to json
		<<"DRAWPATH:", D/bitstring>> ->
			PlayerPid ! {notify_drawn_path, D},
			{reply, {text, tojson(ok)}, Req, PlayerPid};

		_->
			{reply, {text, Data}, Req, PlayerPid}

	end;

websocket_handle({binary, Data}, Req, State) ->
	io:format("BINARY~n"),

	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	io:format("OTHER~n"),
	{ok, Req, State}.



websocket_info({push_update_room, Room}, Req, State) ->
	% io:format("		For player ~p: push_update_room: ~p~n", [State, Room]),
	Name = Room#room.name,
	Msg = #{msg=><<"new_room">>, name=>Name},
	{reply, {text, tojson(Msg)}, Req, State};

websocket_info({push_drawn_path, Data}, Req, State) ->
	% io:format("		For player ~p: push_drawn_path: ~p~n", [State, Data]),
	% Msg = #{msg=><<"path">>, path=>Data},
	Msg = <<"DRAWPATH:", Data/bitstring>>,
	{reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
	io:format("INFO~n"),
	{ok, Req, State}.



websocket_terminate(_Reason, _Req, State) ->
	io:format("TERMINATE -----------------~n"),
	% io:format("State was: ~p~n", [State]),
	case State of
		Pid when is_pid(Pid) ->
			Pid ! destroy;
		_ ->
			ok
	end.


tojson(D) ->
	jiffy:encode(D).
