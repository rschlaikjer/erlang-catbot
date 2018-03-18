-module(catbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    catbot_db:init(),
    catbot_sup:start_link().

stop(_State) ->
    ok.
