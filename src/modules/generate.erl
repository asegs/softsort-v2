-module(generate).
-export([n_objects/1]).

random_from_list(List) ->
  lists:nth(rand:uniform(length(List)), List).

random_in_range([Lower, Upper | _]) ->
  rand:uniform(Upper - Lower) + Lower.

random_sample([], _) ->
  [];
random_sample(_, 0) ->
  [];
random_sample(Set, N) ->
  Picked = random_from_list(Set),
  [Picked] ++ random_sample(lists:delete(Picked, Set), N - 1).

random_element({_, Type, Pattern}) ->
  if Type == <<"list">> -> random_from_list(Pattern);
     Type == <<"math">> -> random_in_range(Pattern);
     Type == <<"set">> -> random_sample(Pattern, rand:uniform(length(Pattern)))
  end.

random_object(Data, N) ->
  [N , list_to_binary(integer_to_list(N)) ] ++ lists:map(fun random_element/1, Data).

n_objects(Structure) ->
  {ok, Names} = maps:find(<<"schema_names">>, Structure),
  {ok, Types} = maps:find(<<"schema_types">>, Structure),
  {ok, Patterns} = maps:find(<<"patterns">>, Structure),
  {ok, N} = maps:find(<<"n">>, Structure),
  Data = lists:zip3(Names, Types, Patterns),
  Options = lists:map(fun(X) -> random_object(Data, X) end, lists:seq(0, N)),
  #{<<"schema_names">> => Names, <<"schema_types">> => Types, <<"options">> => Options}.