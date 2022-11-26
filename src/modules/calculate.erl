-module(calculate).
-export([boolean_to_integer/1, score_list/2, score_membership/2, score_value_range/7]).

boolean_to_integer(true) ->
  1;
boolean_to_integer(false) ->
  0.


score_list(Value, Options) ->
  boolean_to_integer(lists:member(Value, Options)).

score_membership([], _) ->
  1;
score_membership(Desired, Options) ->
  Union = sets:intersection(sets:from_list(Desired), sets:from_list(Options)),
  length(sets:to_list(Union))/length(Desired).


score_value_target(Expected, Actual, Harshness, Spread) ->
  Clean_Harshness = if Harshness == 0 -> 0.1 ; true -> Harshness end,
  Std_Dev = Spread/Clean_Harshness,
  Clean_Std_Dev = if (-1 < Std_Dev) and (Std_Dev < 1) -> 1 ; true -> Std_Dev end,
  math:exp(-1 * math:pow((Actual - Expected),2) / (2 * math:pow(Clean_Std_Dev, 2))).


score_value_range(Actual, Lower, Upper, _, _, _, _) when Lower =< Actual, Actual =< Upper -> 1;
score_value_range(Actual, Lower, Upper, Harshness, Direction, Min, Max) ->
  CloserToUpperEnd = abs(Actual - Lower) < abs(Actual - Upper),
  Result = if
             CloserToUpperEnd -> score_value_target(Upper, Actual, Harshness, Max - Min);
             true -> score_value_target(Lower, Actual, Harshness, Max - Min)
           end,
  if
    Direction == none -> Result ;
    CloserToUpperEnd == (Direction == high) -> math:pow(Result, 1 / Harshness) ;
    true -> math:pow(Result, Harshness)
  end.




