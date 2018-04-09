-module(catbot_reddit).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-compile(export_all).

-define(UPDATE_HEARTBEAT, 600000).
-define(STATS_INTERVAL, 10000).

-include("include/records.hrl").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    bearer_token=""
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    reset_heartbeat(),
    {ok, AccessToken} = get_access_token(),
    spawn_link(fun() -> stats_worker() end),
    {ok, #state{bearer_token = AccessToken}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info(heartbeat, State) ->
    State1 = update_all_subs(State),
    reset_heartbeat(),
    {noreply, State1};
handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Internal functions

reset_heartbeat() ->
    Self = self(),
    spawn(fun() -> timer:sleep(?UPDATE_HEARTBEAT), Self ! heartbeat end).

reddit_config() ->
    {ok, Config} = application:get_env(catbot, reddit),
    Config.
oauth_client_id() -> proplists:get_value(client_id, reddit_config()).
oauth_client_secret() -> proplists:get_value(client_secret, reddit_config()).
user_agent() -> proplists:get_value(user_agent, reddit_config()).

get_basic_auth() ->
    ClientId = oauth_client_id(),
    ClientSecret = oauth_client_secret(),
    base64:encode_to_string(ClientId ++ ":" ++ ClientSecret).

get_access_token() ->
    Url = "https://www.reddit.com/api/v1/access_token",
    AuthStr = get_basic_auth(),
    Headers = [
        {"User-agent", user_agent()},
        {"Authorization", "Basic " ++ AuthStr}
    ],
    ContentType = "application/x-www-form-urlencoded",
    Body = "grant_type=client_credentials",
    Request = {Url, Headers, ContentType, Body},
    HttpOptions = [{autoredirect, false}],
    Options = [{body_format, binary}],
    case httpc:request(post, Request, HttpOptions, Options) of
        {ok, {{_, 503, _}, _RespHeaders, RespBody}} ->
            {error, service_unavailable};
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            Json = jsx:decode(RespBody),
            case proplists:get_value(<<"access_token">>, Json) of
                undefined -> {error, {bad_json, Json}};
                Token when is_binary(Token) -> {ok, binary_to_list(Token)}
            end
    end.

make_api_request(State, Url, Params) ->
    Headers = [
        {"User-agent", user_agent()},
        {"Authorization", "Bearer " ++ State#state.bearer_token}
    ],
    QueryString = binary_to_list(<<
        <<K/binary, "=", V/binary, "&">> || {K, V} <- Params
    >>),
    FinalUrl = Url ++ "?" ++ QueryString,
    Request = {FinalUrl, Headers},
    HttpOptions = [{autoredirect, true}],
    Options = [{body_format, binary}],
    case httpc:request(get, Request, HttpOptions, Options) of
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, 401, _}, _RespHeaders, _RespBody}} ->
            {error, unauthorized};
        {ok, {{_, 503, _}, _RespHeaders, _RespBody}} ->
            {error, service_unavailable}
    end.

update_all_subs(InitialState) ->
    FinalState = lists:foldl(
        fun(Source, State) ->
            _State1 = update_subreddit(State, Source)
        end,
        InitialState,
        catbot_db:get_source_subreddits()
    ),
    FinalState.

stats_worker() ->
    timer:sleep(?STATS_INTERVAL),
    log_stats(),
    stats_worker().

log_stats() ->
    Stats = catbot_db:get_stats(),
    BreedCount = proplists:get_value(breeds, Stats),
    ImageCount = proplists:get_value(images, Stats),
    estatsd:gauge("catbot.stats.tracked_image_count", ImageCount),
    estatsd:gauge("catbot.stats.tracked_breed_count", BreedCount),
    lists:foreach(
        fun({Breed, Count}) ->
            Stat = lists:flatten(io_lib:format(
                "catbot.stats.breeds.~s",
                [binary:replace(Breed, <<" ">>, <<"_">>, [global])]
            )),
            estatsd:gauge(Stat, Count)
        end,
        catbot_db:get_classification_stats()
    ).

update_subreddit(State, Sub=#source_subreddit{}) ->
    % Get the base API url
    NameList = binary_to_list(Sub#source_subreddit.name),
    Url = "https://oauth.reddit.com/r/" ++ NameList ++ "/new",

    % Increase page size
    Params = [
        {<<"limit">>, <<"100">>}
    ],

    % Add 'after' param if known
    Params1 = case Sub#source_subreddit.high_water_mark of
        B when is_binary(B) -> [{<<"before">>, B}|Params];
        _ -> Params
    end,

    case make_api_request(State, Url, Params1) of
        {ok, Json} ->
            Data = proplists:get_value(<<"data">>, Json),
            % io:format("Got data: ~p~n", [Json]),

            % Get the child elements in the list
            Children = proplists:get_value(<<"children">>, Data),

            % If we didn't get any children, there's been nothing new since
            % the last high water mark
            case Children of
                [] -> State;
                _ ->
                    % Extract the data from each subelement
                    ChildData = [
                        proplists:get_value(<<"data">>, Child, []) || Child <- Children
                    ],

                    % Grab the URLs from each subdata
                    ChildUrls = [
                        proplists:get_value(<<"url">>, Datum) || Datum <- ChildData
                    ],

                    % Filter out and junk from URLs that weren't set
                    ValidUrls = lists:filter(
                        fun erlang:is_binary/1,
                        ChildUrls
                    ),

                    % Send the URLs to the image sink
                    Prediction = Sub#source_subreddit.auto_prediction,
                    lists:foreach(
                        fun(U) -> catbot_image_sink:ingest(U, Prediction) end,
                        ValidUrls
                    ),

                    % Grab the first child's name as a highwater
                    FirstChild = hd(Children),
                    FirstChildData = proplists:get_value(<<"data">>, FirstChild, []),
                    FirstChildName = proplists:get_value(<<"name">>, FirstChildData),
                    HighWater = FirstChildName,
                    lager:info("New high water mark for ~s: ~s", [Sub#source_subreddit.name, HighWater]),

                    % Update the highwater on the DB
                    case HighWater of
                        H when is_binary(H) ->
                            catbot_db:set_high_water(Sub#source_subreddit.name, HighWater);
                        _ -> ok
                    end,

                    % Recurse on the new high water mark
                    update_subreddit(State, Sub#source_subreddit{
                        high_water_mark=HighWater
                    })
            end;
        {error, unauthorized} ->
            % Re-up the access token and try again
            {ok, AccessToken} = get_access_token(),
            update_subreddit(State#state{bearer_token=AccessToken}, Sub);
        {error, Reason} ->
            lager:info("Failed to update: ~p~n", [Reason])
    end.
