-module(score).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {Record, Selections, Weights, K} = deriver:get_body(Req),
  {SchemaTypes, Options, Parameters, _} = deriver:get_schema(binary_to_list(Record)),
  Start = os:timestamp(),
  Winners = engine:top_k_scores(SchemaTypes, Options, Selections, Weights, Parameters, K),
  io:format("Took ~p ms\n",[timer:now_diff(os:timestamp(), Start) / 1000]),
  io:format("Served results for ~s\n",[Record]),
  _ = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    jsx:encode(#{<<"winners">> => lists:map(fun tuple_to_list/1, Winners)}),
    Req
  ),
  {ok, Req, State}.