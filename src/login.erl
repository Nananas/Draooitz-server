-module (login).

-export([init_handler/0]).
-export ([start_survivor/0]).

-export([survivor/0]).
-export ([loop/0]).
-export([get_score_of_user/1]).

-define (START_SCORE, 200).

init_handler() ->
	io:format("init~n"),
	Pid = spawn(?MODULE, loop, []),
	register(login_handler, Pid),
	{ok, Pid}.

start_survivor() ->
	Pid = spawn(?MODULE, survivor, []),
	register(survivor, Pid),
	{ok, Pid}.


survivor()->
	io:format("Starting survivor~n"),
	ets:new(t_passes, [ordered_set, public, named_table]),
	ets:insert(t_passes, {<<"foo@bar">>, <<"testing">>}),		% DEBUGGING

	ets:new(t_scores, [ordered_set, public, named_table]),
	ets:insert(t_scores, {<<"foo@bar">>, 273}),		% DEBUGGING
	receive
		stop ->
			ok
		after infinity ->		%% TODO: backup after time? with tab2file?
			ok
	end.
	
loop() ->
	receive
		% TODO: hash i.p.v. pass
		{login, NAME, PASS, Sender} ->
			io:format("Got ~p who wants to login with ~p~n", [NAME, PASS]),
			Hit = ets:lookup(t_passes, NAME),
			io:format("HIT: ~p~n", [Hit]),	

			case Hit of
				[{NAME, PASS}] ->
					Sender ! ok; 
				[{NAME, _}] ->
					Sender ! pass_not_ok;
				[] ->
					create_new_user(NAME, PASS),
					Sender ! created_user
			end,
			loop();
		{_, _, _, Sender} ->
			Sender ! not_found,
			loop()
	end.

create_new_user(N, P) ->
	ets:insert(t_passes, {N, P}),
	ets:insert(t_scores, {N, ?START_SCORE}).

get_score_of_user(N) ->
	[{_,S}|_] = ets:lookup(t_scores, N),
	S.
	