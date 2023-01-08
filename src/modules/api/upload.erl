-module(upload).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  SchemaString = binary_to_list(Schema),
  deriver:write_by_name(Body, SchemaString),
  deriver:derive_schema_options(SchemaString),
  _ = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    <<"">>,
    Req
  ),
  {ok, Req, State}.