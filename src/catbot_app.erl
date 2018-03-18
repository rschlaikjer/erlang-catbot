-module(catbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/oauth/[...]", catbot_http_oauth, []}
        ]}
    ]),
    {ok, ServerInfo} = application:get_env(catbot, http),
    Port = proplists:get_value(port, ServerInfo),
    {ok, _} = cowboy:start_http(
        http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]
    ),
    catbot_sup:start_link().

stop(_State) ->
    ok.
