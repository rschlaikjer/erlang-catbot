-module(catbot_slack_sup).
-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

-export([start_link/0, join_all/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

join_all() ->
    {ok, Slack} = application:get_env(catbot, slack),
    Tokens = proplists:get_value(api_tokens, Slack),
    lists:foreach(
        fun({Name, Token}) ->
            lager:info("Starting slack connection for ~s", [Name]),
            supervisor:start_child(?MODULE, [Token])
        end,
        Tokens
    ).

child_spec() -> {
    catbot_slack,
    {catbot_slack, start_link, []},
    permanent,
    3000,
    worker,
    [catbot_slack]
}.

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [child_spec()]} }.
