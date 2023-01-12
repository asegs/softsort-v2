-module(score).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  {ok, Body, _} = cowboy_req:read_body(Req),
  BodyData = jsx:decode(Body),
  Missing = deriver:get_missing_keys(BodyData),
  if
    length(Missing) > 0 ->
      cowboy_req:reply(
        400,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"missing_keys">> => Missing}),
        Req
      );
    true ->
      {Selections, Weights, K} = deriver:get_body(BodyData),
      {SchemaTypes, Options, Parameters, _} = deriver:get_schema(binary_to_list(Schema)),
      Start = os:timestamp(),
      Winners = engine:top_k_scores(SchemaTypes, Options, Selections, Weights, Parameters, K),
      io:format("Took ~p ms\n",[timer:now_diff(os:timestamp(), Start) / 1000]),
      io:format("Served results for ~s\n",[Schema]),
      _ = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"winners">> => lists:map(fun tuple_to_list/1, Winners)}),
        Req
      )
  end,
  {ok, Req, State}.