-module(database).

-export([start/0]).
-export([test/1]).
-export([add/3]).
-export([nice/1]).
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

count([], Re) -> 
	io:format("Reply compleated! ~p\n",[calendar:universal_time()]),
	"[" ++ Re ++ "]";
count([{Lng, Lat}| Tail], Re) ->
	Amount = integer_to_list(
		length(ets:match(db, {'_','_',{Lng, Lat}}))),

	count(Tail, Re ++ "{" ++ binary_to_list(Lng) ++
	"," ++ binary_to_list(Lat) ++ ":" ++ Amount ++ "}").


nice(List) when length(List) > 90000 ->
	L = lists:flatten(ets:match(db, {'_','_','$1'})),
 	io:format("Starting working on reply... ~p\n",[calendar:universal_time()]),
	T = ets:new(temp,[set]),
	L1 = lists:filter(fun(X) -> ets:insert_new(T, {X,1}) end, L),
	ets:delete(T),
	count(L1, []);
 	


nice(List) -> 
	io:format("Starting working on reply... ~p\n",[calendar:universal_time()]),
	nice(List, []).
nice([], Re) -> 
	io:format("Reply compleated! ~p\n",[calendar:universal_time()]),
	"[" ++ Re ++ "]";
nice([{_,{Lng, Lat},_}| Tail], Re) ->
	nice(Tail, Re ++ "{" ++binary_to_list(Lng) ++":"++binary_to_list(Lat)++"}").


add(Id, Lng, Lat) ->
	[Ln|_] = binary:split(Lng, <<".">>),
	[La|_] = binary:split(Lat, <<".">>),
	ets:insert(db, {Id, {Lng,Lat}, {Ln, La}}),
	ok,
	receive
	after 10000 ->
		ets:match_delete(db, {Id, '_', '_'})
	end.


test(0) -> ok;
test(N) ->
	Lng1 = 57.7118511 - 0.00005,
	Lat1 = 11.9699815 - 0.00005,
	Lng = list_to_binary(io_lib:format("~.7f",[Lng1 + (rand:uniform(999) * 0.00000001)])),
	Lat = list_to_binary(io_lib:format("~.7f",[Lat1 + (rand:uniform(999) * 0.00000001)])),
	database ! {<<"put">>, N, Lng, Lat},
	test(N-1).

	