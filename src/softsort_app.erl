%%%-------------------------------------------------------------------
%% @doc softsort public API
%% @end
%%%-------------------------------------------------------------------

-module(softsort_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    { <<"localhost">>, [{<<"/">>, api, []}] }
  ]),
  {ok, _} = cowboy:start_clear(
    hello_handler,
    [{port, 8081}],
    #{env => #{dispatch => Dispatch}}
  ),
  deriver:derive_schema_options("example"),
  Data = deriver:load_schema_file("example"),
  {ok, SchemaTypes} = maps:find(<<"schema_types">>, Data),
  {ok, Options} = maps:find(<<"options">>, Data),
  {ok, MinMax} = maps:find(<<"parameters">>, Data),
  Weights = [1,1,1,1],
  K = 1,
  Selections = [[<<"SUV">>],{0, 10000, 5, low}, {1980, 2000, 5, high}, [<<"AC">>,<<"Heated seats">>,<<"Cruise Control">>]],
  io:format("~p\n", [engine:top_k_scores(SchemaTypes, Options, Selections, Weights, MinMax, K)]),
  softsort_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
