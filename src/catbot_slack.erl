-module(catbot_slack).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-compile(export_all).

-include_lib("slack_rtm/include/records.hrl").

-define(SLACK_POST_MESSAGE, "https://slack.com/api/chat.postMessage").
-define(BASE_CAT_URL, "https://catbot.rhye.org/").

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    slack_rtm:connect(slack_token()),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info({slack_msg, _From, Message}, State) ->
    State1 = handle_slack_message(State, Message),
    {noreply, State};
handle_info(Info, State) ->
    lager:info("Got info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Internal functions

slack_token() ->
    {ok, Slack} = application:get_env(catbot, slack),
    proplists:get_value(api_token, Slack).

handle_slack_message(State, Message=#slack_rtm_message{}) ->
    Channel = Message#slack_rtm_message.channel,
    User = Message#slack_rtm_message.user,
    Text = Message#slack_rtm_message.text,
    case message_is_for_catbot(Text) of
        false ->
            ok;
        true ->
            handle_cat_request(User, Channel, Text)
    end,
    State;
handle_slack_message(State, _Message) ->
    State.

message_is_for_catbot(<<"catbot,", _/binary>>) -> true;
message_is_for_catbot(<<"<@U9RSC6P50>", _/binary>>) -> true;
message_is_for_catbot(_) -> false.

strip_designator(<<"catbot,", Rest/binary>>) -> Rest;
strip_designator(<<"<@U9RSC6P50>", Rest/binary>>) -> Rest;
strip_designator(Any) -> Any.

strip(<<" ", B/binary>>) -> strip(B);
strip(<<B/binary>>) -> B.

handle_cat_request(User, Channel, Text) ->
    CatType = strip(strip_designator(Text)),
    case CatType of
        <<"list">> ->
            respond_possible_cats(Channel);
        <<"stats">> ->
            respond_stats(Channel);
        _ ->
            lager:info("Asked for cat ~s", [CatType]),
            respond_for_cat(User, Channel, CatType)
    end.

respond_for_cat(User, Channel, CatType) ->
    ActualCatType = make_valid_cat(CatType),
    lager:info("Coerced cat from ~s to ~s", [CatType, ActualCatType]),
    case get_sha_for_cat(ActualCatType) of
        not_found ->
            Resp = lists:flatten(io_lib:format("Sorry, not sure what type of cat '~s' is", [CatType])),
            post_chat_message(Channel, list_to_binary(Resp));
        Sha ->
            CatUrl = ?BASE_CAT_URL ++ Sha,
            Message = case ActualCatType =:= CatType of
                true ->
                    io_lib:format("How about this nice ~s: ~s", [ActualCatType, CatUrl]);
                false ->
                    io_lib:format("Guessing you meant '~s': ~s", [ActualCatType, CatUrl])
            end,
            post_chat_message(Channel, list_to_binary(lists:flatten(Message)))
    end.

make_valid_cat(CatType) ->
    ValidCats = catbot_db:get_known_cat_types(),
    lager:info("Valid cats: ~p", [ValidCats]),
    DistancePairs = [
        {levenshtein:distance(binary_to_list(CatType), binary_to_list(ValidType)), ValidType}
        || ValidType <- ValidCats
    ],
    lager:info("Distance pairs: ~p", [DistancePairs]),
    {BestDistance, BestCat} = lists:foldl(
        fun({D1, C1}, {D2, C2}) ->
            case D1 < D2 of
                true -> {D1, C1};
                false -> {D2, C2}
            end
        end,
        hd(DistancePairs),
        DistancePairs
    ),
    BestCat.

levenshtein(A, []) -> length(A);
levenshtein([], B) -> length(B);
levenshtein([A | TA] = AA, [B | TB] = BA) ->
    lists:min([
        levenshtein(TA, BA) + 1,
        levenshtein(AA, TB) + 1,
        levenshtein(TA, TB) + lev_delta(A, B)
    ]).

lev_delta(_A, _A) -> 0;
lev_delta(_A, _B) -> 1.

get_sha_for_cat(Cat) ->
    catbot_db:get_image_for_cat_type(Cat).

respond_possible_cats(Channel) ->
    Cats = lists:sort(catbot_db:get_known_cat_types()),
    CatsListBin = << <<C/binary, ", ">> || C <- Cats>>,
    Message = <<"I know about the following cats: ", CatsListBin/binary>>,
    post_chat_message(Channel, Message).

respond_stats(Channel) ->
    Stats = catbot_db:get_stats(),
    BreedCount = proplists:get_value(breeds, Stats),
    ImageCount = proplists:get_value(images, Stats),
    Message = list_to_binary(lists:flatten(io_lib:format(
        "Currently indexing ~p images across ~p breeds",
        [ImageCount, BreedCount]
    ))),
    post_chat_message(Channel, Message).

post_chat_message(Channel, Text) ->
    Json = jsx:encode([
        {<<"channel">>, Channel},
        {<<"text">>, Text},
        {<<"as_user">>, true},
        {<<"username">>, <<"catbot">>}
    ]),
    RtmToken = slack_token(),
    Headers = [
       {"authorization", binary_to_list(<<"Bearer ", RtmToken/binary>>)}
    ],
    Type = "application/json",
    Result = httpc:request(post, {?SLACK_POST_MESSAGE, Headers, Type, Json}, [], []),
    case Result of
        {ok, {{_HttpVer, 200, _Msg}, _ResponseHeaders, _ResponseBody}} ->
            ok;
        {ok, {{_HttpVer, _Code, _Msg}, _ResponseHeaders, ResponseBody}} ->
            lager:info("Unexpected response from API: ~p~n", [ResponseBody]),
            ok
    end.
