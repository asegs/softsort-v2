-module(parameterize).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  deriver:derive_schema_options(binary_to_list(Schema)),
  io:format("Derived schema for ~s\n",[Schema]),
  Req1 = cowboy_req:reply(
    204,
    #{<<"content-type">> => <<"application/json">>},
    <<"">>,
    Req
  ),
  {ok, Req1, State}.