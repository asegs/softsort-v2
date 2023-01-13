-module(schema).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  {Types, _ , Parameters, Names} = deriver:get_schema(binary_to_list(Schema)),
  io:format("Served schema for ~s\n",[Schema]),
  Req1 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    jsx:encode(#{<<"types">> => Types, <<"parameters">> => Parameters, <<"names">> => Names}),
    Req
  ),
  {ok, Req1, State}.