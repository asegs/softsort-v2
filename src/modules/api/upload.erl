-module(upload).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  SchemaString = binary_to_list(Schema),
  deriver:write_by_name(Body, SchemaString),
  deriver:derive_schema_options(SchemaString),
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
  Req3 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    <<"">>,
    Req2
  ),
  {ok, Req3, State}.