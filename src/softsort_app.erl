%%%-------------------------------------------------------------------
%% @doc softsort public API
%% @end
%%%-------------------------------------------------------------------

-module(softsort_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    { '_',
      [
        {<<"/:schema">>, score, []},
        {<<"/schema/:schema">>, schema, []},
        {<<"/upload/:schema">>, upload, []},
        {<<"/sample/:schema">>, sample, []},
        {<<"/schema/:schema/parameterize">>, parameterize, []},
        {"/[...]", cowboy_static, {priv_dir, softsort, "/"}}
      ]
    }
  ]),
  {ok, _} = cowboy:start_clear(
    cowboy_handler,
    [{port, 8081}],
    #{
        env => #{dispatch => Dispatch},
        middlewares => [cors_middleware, cowboy_router, cowboy_handler]
      }
  ),
  softsort_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
