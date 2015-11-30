-module(socket_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
	player_id
}).

init(_, _, _) ->
	io:format("INIT~n"),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
	Req2 = cowboy_req:compact(Req),
	io:format("WS INIT~n"),
	{ok, Req2, #state{}}.

websocket_handle({text, Data}, Req, State) ->
	io:format("TEXT~n"),
	io:format("STATE: ~p ~n", [State]),
	case Data of
		<<"LOGIN:",D/bitstring>>->
			handle_login(D, Req, State);

		_->
			{reply, {text, Data}, Req, State}
	end;

websocket_handle({binary, Data}, Req, State) ->
	io:format("BINARY~n"),
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	io:format("OTHER~n"),
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	io:format("INFO~n"),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	io:format("TERMINATE~n"),
	ok.



handle_login(D, Req, State) ->
	{Comma_pos, _} = binary:match(D, [<<",">>]),
	F = binary:part(D, {0, Comma_pos}),
	S = binary:part(D, {Comma_pos+1, byte_size(D) - (Comma_pos+1)}),

	login_handler ! {login, F, S, self()},

	receive
		ok -> 
			Score = login:get_score_of_user(F),
			Out = convert_to_json("LOGGED_IN", Score),
			{reply, {text, Out}, Req, #state{player_id=F}};
		pass_not_ok ->
			{reply, {text, convert_to_json("WRONG_PASS")}, Req, State};
		created_user -> 
			Score = login:get_score_of_user(F),
			{reply, {text, convert_to_json("REGISTERED_LOGGED_IN", Score)}, Req, #state{player_id=F}};
		_ ->
			{reply, {text, convert_to_json("OTHER_ERROR")}, Req, State}
	end;

convert_to_json(State) ->
	"{ \"state\":\"" ++ State ++ "\"}".
convert_to_json(State, Score) ->
	io:format("Score~p~n", [Score]),
	"{ \"state\":\"" ++ State ++ "\", \"s\":" ++ integer_to_list(Score) ++ "}".
