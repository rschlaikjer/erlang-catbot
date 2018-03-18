-module(catbot_reddit).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-compile(export_all).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    bearer_token
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{bearer_token="OlucnzT39D2yXRVR4Z_QQ0ebSlg"}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Internal functions

reddit_config() -> application:get_env(catbot, reddit).
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
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            Json = jsx:decode(RespBody),
            case proplists:get_value(<<"access_token">>, Json) of
                undefined -> {error, {bad_json, Json}};
                Token when is_binary(Token) -> {ok, Token}
            end
    end.

make_api_request(State, Url) ->
    Headers = [
        {"User-agent", user_agent()},
        {"Authorization", "Bearer " ++ State#state.bearer_token}
    ],
    Request = {Url, Headers},
    HttpOptions = [{autoredirect, false}],
    Options = [{body_format, binary}],
    case httpc:request(get, Request, HttpOptions, Options) of
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            {ok, jsx:decode(RespBody)}
    end.

get_link_urls(State) ->
    {ok, Json} = make_api_request(State, "https://oauth.reddit.com/r/cats/new"),
    Data = proplists:get_value(<<"data">>, Json),
    Children = proplists:get_value(<<"children">>, Data),
    ChildData = [
        proplists:get_value(<<"data">>, Child, []) || Child <- Children
    ],
    ChildUrls = [
        proplists:get_value(<<"url">>, Datum) || Datum <- ChildData
    ],
    lists:filter(
        fun erlang:is_binary/1,
        ChildUrls
    ).

is_image_url(Url) ->
    case httpc:request(head, {Url, []}, [], []) of
        {ok, {{_, 200, _}, Headers, _}} ->
            ContentType = proplists:get_value("content-type", Headers),
            is_content_type_image(ContentType);
        _ -> false
    end.

is_content_type_image(Type) when is_list(Type) ->
    is_content_type_image(list_to_binary(Type));
is_content_type_image(<<"image/", _Rest/binary>>) -> true;
is_content_type_image(_) -> false.
