%%%-------------------------------------------------------------------
%% @doc softsort public API
%% @end
%%%-------------------------------------------------------------------

-module(softsort_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    { <<"localhost">>, [{<<"/">>, score, []}, {<<"/schema/:schema">>, schema, []}] }
  ]),
  {ok, _} = cowboy:start_clear(
    no_handler,
    [{port, 8081}],
    #{env => #{dispatch => Dispatch}}
  ),
  softsort_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
