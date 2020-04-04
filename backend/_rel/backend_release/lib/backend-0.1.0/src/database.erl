-module(database).

-export([start/0]).
% -export([fetch/2]).
% -export([test/1]).
-export([add/3]).

-export([init/0]).

start() -> 
	Pid = spawn_link(?MODULE, init, []),
	register(database,Pid),
	{ok, Pid}.

init() ->
	ets:new(db, [set, named_table, public,{write_concurrency, true}, 
			{read_concurrency, true}]),
	loop([]).

loop(Data) ->
	receive
		{<<"get">>, <<"all">>, Pid} -> Pid ! nice(ets:tab2list(db));
		%{<<"get">>, Data, Pid} -> Pid ! fetch(Data, ets:tab2list(db));
		{<<"put">>, Id, Lng, Lat} ->
			spawn(?MODULE, add, [Id, Lng, Lat])
			%ets:insert(db, {Id, {Lng,Lat}})
	end, loop(Data).

nice(List) -> nice(List, []).
nice([], Re) -> "[" ++ Re ++ "]";
nice([{_,{Lng, Lat}}| Tail], Re) ->
	nice(Tail, Re ++ "{" ++binary_to_list(Lng) ++":"++binary_to_list(Lat)++"}").


add(Id, Lng, Lat) ->
	ets:insert(db, {Id, {Lng,Lat}}),
	receive
	after 1000*60 ->
		ets:match_delete(db, {Id, '_'})
	end.




% fetch(Data, List) -> fetch(Data, List, "[").
% fetch(_, [], Re) -> Re ++ "]";
% fetch([Lng1, Lng2, Lat1, Lat2], [{_, {Ln, La}}|Rest], Re) when 
% 	(Lng1 < Ln andalso
% 	Lng2 > Ln andalso 
% 	Lat1 < La andalso 
% 	Lat2 > La) ->
% 		fetch([Lng1, Lng2, Lat1, Lat2], Rest, Re ++ "{" ++ io_lib:format("~.5f",[Ln]) ++ ":" ++ io_lib:format("~.5f",[La]) ++ "}");
% fetch(Data, [_|Rest], Re) -> fetch(Data, Rest, Re).

% test(0) -> ok;
% test(N) ->
% 	Lng = list_to_binary(io_lib:format("~.5f",[57.7 + (rand:uniform(9999) * 0.00001)])),
% 	Lat = list_to_binary(io_lib:format("~.5f",[11.9 + (rand:uniform(9999) * 0.00001)])),
% 	database ! {<<"put">>, N, Lng, Lat},
% 	receive
% 	after 1000 ->
% 		ok
% 	end,
% 	test(N-1).

	