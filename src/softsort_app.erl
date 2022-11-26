%%%-------------------------------------------------------------------
%% @doc softsort public API
%% @end
%%%-------------------------------------------------------------------

-module(softsort_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  deriver:derive_schema_options("example"),
    softsort_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
