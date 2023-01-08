-module(deriver).
-export([derive_schema_options/1, get_schema/1, get_body/1, write_by_name/2]).

unique(List) ->
  sets:to_list(sets:from_list(List)).

%Derive based on accumulators for each type.
%Since zipping options just gives an entry.
derive_schema_option({Schema_Type, Accumulated_Option}) ->
  if
    Schema_Type == list -> unique(Accumulated_Option);
    Schema_Type == set -> unique(lists:flatten(Accumulated_Option, []));
    true -> [lists:min(Accumulated_Option), lists:max(Accumulated_Option)]
  end.


accumulate_option(Lists, Option) ->
  [_,_ | Fields] = Option,
  lists:map(fun ({List, Element}) -> [Element | List] end, lists:zip(Lists, Fields)).

accumulate_options(Lists,[]) ->
  Lists;
accumulate_options(Lists,[Option | Options]) ->
  accumulate_options(accumulate_option(Lists, Option), Options).

schema_to_atoms(Schema) ->
  lists:map(fun binary_to_atom/1, Schema).

derive_schema_options(Name) ->
  Table = load_schema_file(Name),
  {ok, Schema_Text} = maps:find(<<"schema_types">>, Table),
  Schema_Types = schema_to_atoms(Schema_Text),
  {ok, Options} = maps:find(<<"options">>, Table),
  Accumulated = accumulate_options(lists:map(fun(_) -> [] end, Schema_Types), Options),
  Data = lists:zip(Schema_Types, Accumulated),
  Params = lists:map(fun derive_schema_option/1, Data),
  Derived = maps:put(<<"parameters">>, Params, Table),
  write_schema_file(Name, Derived).

load_schema_file(Name) ->
  {ok, File} = file:read_file("records/"++Name++".json"),
  jsx:decode(File).

write_by_name(Binary, Name) ->
  file:write_file("records/" ++ Name ++ ".json", Binary).

write_schema_file(Name, Json) ->
  write_by_name(jsx:encode(Json), Name).

get_schema(Name) ->
  Data = load_schema_file(Name),
  {ok, SchemaTypes} = maps:find(<<"schema_types">>, Data),
  {ok, Options} = maps:find(<<"options">>, Data),
  {ok, Parameters} = maps:find(<<"parameters">>, Data),
  {ok, Names} = maps:find(<<"schema_names">>, Data),
  {SchemaTypes, Options, Parameters, Names}.

get_body(Req) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  BodyData = jsx:decode(Body),
  {ok, Record} = maps:find(<<"record">>, BodyData),
  {ok, Selections} = maps:find(<<"selections">>, BodyData),
  {ok, Weights} = maps:find(<<"weights">>, BodyData),
  {ok, K} = maps:find(<<"k">>, BodyData),
  {Record, Selections, Weights, K}.

