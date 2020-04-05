-module(toppage_h).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	{ok, [{Data1, _}], _} = cowboy_req:read_urlencoded_body(Req0),
	%% data is [{<<"{\"lat\":57.6963947,\"lng\":11.9357684,\"id\":\"0874b3f0-7726-11ea-b707-4521c2a2c50a\"}">>,true}]
	Data = jsone:decode(Data1, [{object_format, proplist}]),
	Req = echo(Method, Data, Req0),
	{ok, Req, Opts}.

echo(<<"POST">>, [{<<"scope">>, GeoList}], Req) ->
	database ! {<<"get">>, GeoList, self()},
	receive 
		Return -> 
			cowboy_req:reply(200, #{
				<<"content-type">> => <<"application/json; charset=utf-8">>
			}, Return, Req)
	end;

echo(<<"POST">>, Data, Req) ->
	Lng = proplists:get_value(<<"lng">>, Data),
	Lat = proplists:get_value(<<"lat">>, Data),
	Id = proplists:get_value(<<"id">>, Data),

	database ! {<<"put">>, Id, Lng, Lat},

	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, <<"ok">>, Req);

echo(<<"GET">>, _, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, <<"GET request, not currently working">>, Req);

echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
