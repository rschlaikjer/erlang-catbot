-module(catbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    catbot_db:init(),
    {ok, Pid} = catbot_sup:start_link(),
    catbot_slack_sup:join_all(),
    {ok, Pid}.


stop(_State) ->
    ok.
