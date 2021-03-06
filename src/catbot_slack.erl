-module(catbot_slack).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-compile(export_all).

-include_lib("slack_rtm/include/records.hrl").

-define(SLACK_POST_MESSAGE, "https://slack.com/api/chat.postMessage").
-define(BASE_CAT_URL, "https://catbot.rhye.org/").

-define(AFFIRMATIVE_RESPONSES, [
    "How about this nice ~s (confidence: ~p): ~s",
    "Sure, pretty sure this photo is of a ~s (confidence: ~p): ~s",
    "Found this ~s (confidence: ~p): ~s",
    "What do you think of this ~s (confidence: ~p): ~s",
    "~s? Sure (confidence: ~p): ~s",
    "One ~s coming right up (confidence: ~p): ~s"
]).

-define(CORRECTED_RESPONSES, [
    "How about this nice ~s instead (confidence: ~p): ~s",
    "Pretty sure this ~s is close to what you want (confidence: ~p): ~s",
    "Hmm, I found this ~s instead (confidence: ~p): ~s",
    "What do you think of this ~s instead (confidence: ~p): ~s",
    "Is ~s ok? (confidence: ~p) ~s"
]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    slack_token,
    user_id,
    quoted_user_id
}).

start_link(Token) ->
    gen_server:start_link(?MODULE, [Token], []).

init([SlackToken]) ->
    lager:info("Starting slack worker with token ~p", [SlackToken]),
    slack_rtm:connect(SlackToken),
    {ok, #state{slack_token=SlackToken}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info({slack_connected, _From, UserId}, State) ->
    QuotedId = <<"<@", UserId/binary, ">">>,
    {noreply, State#state{user_id=UserId, quoted_user_id=QuotedId}};
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

handle_slack_message(State, Message=#slack_rtm_message{}) ->
    Channel = Message#slack_rtm_message.channel,
    User = Message#slack_rtm_message.user,
    Text = Message#slack_rtm_message.text,
    case message_is_for_catbot(State, Text, Channel, User) of
        false ->
            ok;
        true ->
            spawn(fun() -> handle_cat_request(State, User, Channel, Text) end)
    end,
    State;
handle_slack_message(State, _Message) ->
    State.

% Ignore messages catbot sent itself
message_is_for_catbot(#state{user_id=Uid}, _, _, Uid) -> false;
% Plain to accept
message_is_for_catbot(_, <<"catbot,", _/binary>>, _, Msg) when is_binary(Msg) -> true;
message_is_for_catbot(_, <<"Catbot,", _/binary>>, _, Msg) when is_binary(Msg) -> true;
message_is_for_catbot(_, <<"CATBOT,", _/binary>>, _, Msg) when is_binary(Msg) -> true;
% If the channel begins with D it's a DM, assume we're being talked at
message_is_for_catbot(_, _Msg, <<"D", _/binary>>, Msg) when is_binary(Msg) -> true;
% Anything else is probably not for us
message_is_for_catbot(#state{quoted_user_id=Qid}, Msg, _Channel, _User) when is_binary(Msg) ->
    case binary:match(Msg, Qid) of
        {0, L} -> true;
        _ -> false
    end;
message_is_for_catbot(_, _, _, _) -> false.

strip_designator(_, <<"catbot,", Rest/binary>>) -> Rest;
strip_designator(_, <<"Catbot,", Rest/binary>>) -> Rest;
strip_designator(_, <<"CATBOT,", Rest/binary>>) -> Rest;
strip_designator(#state{quoted_user_id=Qid}, Message) when is_binary(Message) ->
    case binary:match(Message, Qid) of
        {0, L} -> binary:part(Message, L, byte_size(Message) - L);
        nomatch -> Message
    end;
strip_designator(_, Msg) -> Msg.

strip(<<" ", B/binary>>) -> strip(B);
strip(<<B/binary>>) -> B.

handle_cat_request(State, User, Channel, Text) ->
    CatType = strip(strip_designator(State, Text)),
    case CatType of
        <<"list">> ->
            respond_possible_cats(State, Channel);
        <<"stats">> ->
            respond_stats(State, Channel);
        <<"random">> ->
            respond_random(State, Channel);
        <<"breakdown">> ->
            respond_breakdown(State, Channel);
        <<"help">> ->
            respond_help(State, Channel);
        <<"evict ", Query/binary>> ->
            respond_evict(State, Channel, User, Query);
        _ ->
            respond_for_cat(State, User, Channel, CatType)
    end.

respond_evict(State, Channel, User, Query) ->
    case catbot_db:is_user_admin(User) of
        false ->
            post_chat_message(
                State, Channel,
                <<"Sorry, <@", User/binary, "> is not in the sudoers file">>
            );
        true ->
            Sha = strip(Query),
            Msg = case catbot_db:delete_image(Sha) of
                true -> <<"Ok, deleted image ", Sha/binary>>;
                false -> <<"Couldn't find any image with a SHA of ", Sha/binary>>
            end,
            post_chat_message(State, Channel, Msg)
    end.

respond_help(State, Channel) ->
    Header = <<"You can address me by starting a message with either `@catbot` or `catbot,` and following it with either a command or the name of a breed. You can also DM me, in which case you don't need to address me by name!\nI support the following commands:\n">>,
    Cmds = [
        <<"- random: Special keyword that selects an image at random">>,
        <<"- list: List all known breeds of cat">>,
        <<"- stats: Print statstics on my dataset">>,
        <<"- breakdown: Detailed statistics on each breed">>,
        <<"- evict <sha>: Remove an offending image from the database">>,
        <<"- help: This help message">>
    ],
    CmdList = << <<Cmd/binary, "\n">> || Cmd <- Cmds >>,
    Help = <<Header/binary, CmdList/binary>>,
    post_chat_message(State, Channel, Help).

respond_for_cat(State, User, Channel, CatType) ->
    ActualCatType = make_valid_cat(CatType),
    lager:info("Coerced cat from ~s to ~s", [CatType, ActualCatType]),
    case catbot_db:get_image_for_cat_type(ActualCatType) of
        not_found ->
            Resp = lists:flatten(io_lib:format("Sorry, not sure what type of cat '~s' is", [CatType])),
            post_chat_message(State, Channel, list_to_binary(Resp));
        {Sha, Confidence} ->
            <<ShaPrefix:2/binary, ShaRest/binary>> = Sha,
            CatUrl = ?BASE_CAT_URL ++ binary_to_list(ShaPrefix) ++ "/" ++ binary_to_list(ShaRest),
            Message = case ActualCatType =:= CatType of
                true -> random_response(?AFFIRMATIVE_RESPONSES, ActualCatType, Confidence, CatUrl);
                false -> random_response(?CORRECTED_RESPONSES, ActualCatType, Confidence, CatUrl)
            end,
            post_chat_message(State, Channel, list_to_binary(lists:flatten(Message)))
    end.

respond_random(State, Channel) ->
    % Pick an image totally at random
    {Sha, Breed, Confidence} = catbot_db:get_random_image(),
    <<ShaPrefix:2/binary, ShaRest/binary>> = Sha,
    CatUrl = ?BASE_CAT_URL ++ binary_to_list(ShaPrefix) ++ "/" ++ binary_to_list(ShaRest),
    Resp = case Breed of
        null ->
            lists:flatten(io_lib:format("Not sure what this is, but here: ~s", [CatUrl]));
        _ ->
            lists:flatten(io_lib:format("Rolled the dice, got a ~s cat (confidence ~p): ~s", [Breed, Confidence, CatUrl]))
    end,
    post_chat_message(State, Channel, list_to_binary(Resp)).

random_response(ResponseOptions, CatType, Confidence, Url) ->
    Index = rand:uniform(length(ResponseOptions)),
    FormatString = lists:nth(Index, ResponseOptions),
    io_lib:format(FormatString, [CatType, Confidence, Url]).

make_valid_cat(CatTypeIn) ->
    % Get our known breeds
    ValidCats = catbot_db:get_known_cat_types(),

    % Lowercase the input
    CatType = list_to_binary(string:to_lower(binary_to_list(CatTypeIn))),

    % Check if the cat was already valid
    case lists:any(
        fun(C) -> C =:= CatType end,
        ValidCats
    ) of
        % If the type is already in the list, don't need to do
        % leven
        true -> CatType;
        false ->
            DistancePairs = [
                {levenshtein:levenshtein(CatType, ValidType), ValidType}
                || ValidType <- ValidCats
            ],
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
            BestCat
    end.

respond_possible_cats(State, Channel) ->
    Cats = lists:sort(catbot_db:get_known_cat_types()),
    CatsListBin = << <<C/binary, ", ">> || C <- Cats>>,
    Message = <<"I know about the following cats: ", CatsListBin/binary>>,
    post_chat_message(State, Channel, Message).

respond_breakdown(State, Channel) ->
    FullStats = catbot_db:get_classification_stats(),

    % Get the longest name to calculate padding
    MaxNameLen = lists:foldl(
        fun(Name, PrevLongest) ->
            case byte_size(Name) > PrevLongest of
                true -> byte_size(Name);
                false -> PrevLongest
            end
        end,
        0,
        [Name || {Name, _Count} <- FullStats]
    ),

    % Pad all the names to match
    FormatPairs = lists:foldl(
        fun({Name, Count}, Acc) ->
            Padding = << <<" ">> || _ <- lists:seq(1, MaxNameLen-byte_size(Name))>>,
            Tuple = {<<Name/binary, Padding/binary>>, list_to_binary(io_lib:format("~10.10. B", [Count]))},
            [Tuple|Acc]
        end,
        [],
        FullStats
    ),

    ClassificationData = <<
        <<N/binary, C/binary, "\n">> || {N, C} <- lists:reverse(FormatPairs)
    >>,

    Message = list_to_binary(lists:flatten(io_lib:format(
        "Image classification stats:~n```~n~s~n```",
        [ClassificationData]
    ))),
    post_chat_message(State, Channel, Message).

respond_stats(State, Channel) ->
    Stats = catbot_db:get_stats(),
    BreedCount = proplists:get_value(breeds, Stats),
    ImageCount = proplists:get_value(images, Stats),
    TotalImageSize = proplists:get_value(bytes, Stats),
    SizeString = io_lib:format("~.1f GiB", [TotalImageSize / (1024 * 1024 * 1024)]),
    Message = list_to_binary(lists:flatten(io_lib:format(
        "Currently indexing ~p images (~s) across ~p breeds",
        [ImageCount, SizeString, BreedCount]
    ))),
    post_chat_message(State, Channel, Message).

post_chat_message(State, Channel, Text) ->
    Json = jsx:encode([
        {<<"channel">>, Channel},
        {<<"text">>, Text},
        {<<"as_user">>, true},
        {<<"username">>, <<"catbot">>}
    ]),
    RtmToken = State#state.slack_token,
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
