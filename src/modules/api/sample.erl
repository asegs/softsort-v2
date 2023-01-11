-module(sample).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  {ok, Body, _} = cowboy_req:read_body(Req),
  Structure = jsx:decode(Body),
  Data = generate:n_objects(Structure),
  deriver:write_by_name(jsx:encode(Data), binary_to_list(Schema)),
  deriver:derive_schema_options(binary_to_list(Schema)),
  io:format("Generated samples for ~s\n",[Schema]),
  _ = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    jsx:encode(#{<<"data">> => Data}),
    Req
  ),
  {ok, Req, State}.