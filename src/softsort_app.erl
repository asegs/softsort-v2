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
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),
  deriver:derive_schema_options("example"),
    softsort_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
