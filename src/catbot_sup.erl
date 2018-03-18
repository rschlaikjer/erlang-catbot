-module(catbot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
    {
     catbot_image_sink,
     {catbot_image_sink, start_link, []},
     permanent,
     3000,
     worker,
     [catbot_image_sink]
    }
    ],
    {ok, { {one_for_one, 5, 10}, Children} }.
