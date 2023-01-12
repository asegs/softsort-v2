-module(score).
-behaviour(cowboy_handler).
-export([init/2]).

init( Req, State ) ->
  {ok, Schema} = maps:find(schema,cowboy_req:bindings(Req)),
  {ok, Body, _} = cowboy_req:read_body(Req),
  BodyData = jsx:decode(Body),
  Missing = deriver:get_missing_keys(BodyData),
  {Code, Message} = if
    length(Missing) > 0 -> {400, jsx:encode(#{<<"error">> => <<"missing_keys">>,<<"details">> => Missing})};
    true ->
      {Selections, Weights, K} = deriver:get_body(BodyData),
      {SchemaTypes, Options, Parameters, Names} = deriver:get_schema(binary_to_list(Schema)),
      Mismatches = deriver:get_mismatched_types(SchemaTypes, Selections, Names),
      if
        length(Selections) =/= length(Weights) -> {400, jsx:encode(#{<<"error">> => <<"mismatched_request_size">>,<<"details">> => [length(Selections), length(Weights)]})};
        length(Options) == 0 -> {404, jsx:encode(#{<<"error">> => <<"no_options">>, <<"details">> => <<"">>})};
        length(Selections) =/= length(SchemaTypes) -> {400, jsx:encode(#{<<"error">> => <<"mismatched_data_size">>, <<"details">> => [length(Selections), length(SchemaTypes)]})};
        length(Mismatches) > 0 -> {400, jsx:encode(#{<<"error">> => <<"malformed_selection_data">>, <<"details">> => Mismatches})};
        true ->
          Start = os:timestamp(),
          Winners = engine:top_k_scores(SchemaTypes, Options, Selections, Weights, Parameters, K),
          io:format("Took ~p ms\n",[timer:now_diff(os:timestamp(), Start) / 1000]),
          io:format("Served results for ~s\n",[Schema]),
          {200, jsx:encode(#{<<"winners">> => lists:map(fun tuple_to_list/1, Winners)})}
      end
  end,
  _ = cowboy_req:reply(
    Code,
    #{<<"content-type">> => <<"application/json">>},
    Message,
    Req
  ),
  {ok, Req, State}.