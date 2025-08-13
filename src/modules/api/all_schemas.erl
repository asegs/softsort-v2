-module(all_schemas).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  Schemas = deriver:get_all_schemas(),
  io:format("Served all schemas\n",[]),
  Req1 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    jsx:encode(#{<<"schemas">> => Schemas}),
    Req
  ),
  {ok, Req1, State}.