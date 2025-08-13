-module(deriver).
-export([derive_schema_options/1, get_schema/1, get_body/1, write_by_name/2, get_missing_keys/1, get_mismatched_types/3, get_all_schemas/0]).

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

get_all_schemas() ->
  PrivDir = code:priv_dir(softsort),
  RecordsDir = filename:join([PrivDir, "records"]),
  {ok, Records} = file:list_dir(RecordsDir),
  NoFileNameRecords = lists:map(fun filename:rootname/1, Records),
  lists:map(fun list_to_binary/1, NoFileNameRecords).

load_schema_file(Name) ->
  PrivDir = code:priv_dir(softsort),
  FilePath = filename:join([PrivDir, "records", Name ++ ".json"]),
  {ok, File} = file:read_file(FilePath),
  jsx:decode(File).

write_by_name(Binary, Name) ->
  PrivDir = code:priv_dir(softsort),
  FilePath = filename:join([PrivDir, "records", Name ++ ".json"]),
 file:write_file(FilePath, Binary).

write_schema_file(Name, Json) ->
  write_by_name(jsx:encode(Json), Name).

get_schema(Name) ->
  Data = load_schema_file(Name),
  {ok, SchemaTypes} = maps:find(<<"schema_types">>, Data),
  {ok, Options} = maps:find(<<"options">>, Data),
  {ok, Parameters} = maps:find(<<"parameters">>, Data),
  {ok, Names} = maps:find(<<"schema_names">>, Data),
  {SchemaTypes, Options, Parameters, Names}.

get_missing_keys(BodyData) ->
  Required = sets:from_list([<<"selections">>,<<"weights">>,<<"k">>]),
  Found = sets:from_list(maps:keys(BodyData)),
  sets:to_list(sets:subtract(Required, Found)).

is_list_schema(List) ->
  is_list(List) andalso lists:all(fun is_binary/1, List).

is_math_schema(List) ->
  is_list(List) andalso (length(List) == 4) andalso lists:all(fun is_number/1, List).

is_set_schema(List) ->
  is_list(List) andalso lists:all(fun is_binary/1, List).

get_mismatch_data({Type, Entered, Name}) ->
  case Type of
    <<"list">> -> [is_list_schema(Entered), <<"List types require a list of strings.">>, Name];
    <<"math">> -> [is_math_schema(Entered), <<"Math types require a list of 4 numbers.">>, Name];
    <<"set">> -> [is_set_schema(Entered), <<"Set types require a list of strings.">>, Name]
  end.

get_mismatched_types(Types, ProvidedData, Names) ->
  Meta = lists:zip3(Types, ProvidedData, Names),
  lists:map(fun ([_ | Data]) -> Data end, lists:filter(fun (Data) -> not lists:nth(1, Data) end, lists:map(fun get_mismatch_data/1, Meta))).

get_body(BodyData) ->
  {ok, Selections} = maps:find(<<"selections">>, BodyData),
  {ok, Weights} = maps:find(<<"weights">>, BodyData),
  {ok, K} = maps:find(<<"k">>, BodyData),
  {Selections, Weights, K}.

