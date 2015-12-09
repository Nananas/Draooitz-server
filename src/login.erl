%% @author Thomas Dendale
%% @doc Login handler. Starts a survivor with the tables from Rooms and Players

-module (login).

-export ([start_survivor/0]).
-export([survivor/0]).
-export ([handle_login/1]).


start_survivor() ->
	Pid = spawn(?MODULE, survivor, []),
	register(survivor, Pid),
	{ok, Pid}.


% @private
survivor()->
	io:format("Starting survivor: players dets & rooms ets~n"),

	players:start(),
	players:create_new_player(<<"foo@bar">>, <<"testing">>),
	rooms:start(),

	receive
		stop ->
			ok
		after infinity ->
			ok
	end.


% @doc 
% @spec handle_login(bitstring()) -> {ok, Player} | not_ok
% 		Player = #player{}
handle_login(D) ->
	{Comma_pos, _} = binary:match(D, [<<",">>]),
	F = binary:part(D, {0, Comma_pos}),
	S = binary:part(D, {Comma_pos+1, byte_size(D) - (Comma_pos+1)}),

	case players:check_player_exists(F) of
		{ok, Player} ->		%% user found, check pass
			case players:check_pass(Player, S) of
				ok ->
					{ok, Player};
				not_ok ->
					not_ok
			end;

		not_found ->
			io:format("Created new user ~n"),
			NewPlayer = players:create_new_player(F, S),
			{ok, NewPlayer};

		error ->
			not_ok
	end.

