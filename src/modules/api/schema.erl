-module(schema).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  io:format("~p\n",[cowboy_req:bindings(Req)]),
  Req_1 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"hello, world">>,
    Req
  ),
  {ok, Req, State}.