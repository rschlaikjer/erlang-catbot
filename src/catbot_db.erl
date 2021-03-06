-module(catbot_db).
-compile(export_all).

-include("include/records.hrl").

-define(POOL_NAME, catbot_db_pool).

init() ->
    {ok, DbInfo} = application:get_env(catbot, database),
      Password = proplists:get_value(password, DbInfo),
      Username = proplists:get_value(username, DbInfo),
      Host = proplists:get_value(host, DbInfo),
      Database = proplists:get_value(database, DbInfo),
      pgapp:connect(
          ?POOL_NAME, [
              {size, 10},
              {host, Host},
              {database, Database},
              {username, Username},
              {password, Password}
          ]
      ),
      ok.

unix_to_gregorian(Time) when is_integer(Time) ->
    calendar:gregorian_seconds_to_datetime(Time + 62167219200).

safe_unix_to_gregorian(Time) when is_integer(Time) ->
    unix_to_gregorian(Time);
safe_unix_to_gregorian(Other) ->
    Other.

ingest_image(Sha, OriginalUrl, SizeBytes) ->
    case pgapp:equery(
        ?POOL_NAME,
        "INSERT INTO images
        (sha, original_url, size_bytes)
        VALUES
        ($1, $2, $3)",
        [Sha, OriginalUrl, SizeBytes]
    ) of
        {ok, 1} -> ok;
        {error,{error,error,_,unique_violation,_,_}} -> ok
    end.

get_source_subreddits() ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT *
        FROM source_subreddits",
        []
    ) of
        {ok, RowSpec, Rows} ->
            RowProps = rows_to_proplists(RowSpec, Rows),
            [#source_subreddit{
                name=proplists:get_value(<<"name">>, Row),
                high_water_mark=proplists:get_value(<<"high_water_mark">>, Row),
                auto_prediction=proplists:get_value(<<"auto_prediction">>, Row)
            } || Row <- RowProps]
    end.

rows_to_proplists(RowSpec, Rows) ->
    ColNames = [element(2, Spec) || Spec <- RowSpec],
    [lists:zip(ColNames, [element(I,Row) || I <- lists:seq(1,tuple_size(Row))])
     || Row <- Rows].

set_high_water(Name, HighWater) ->
    case pgapp:equery(
        ?POOL_NAME,
        "UPDATE source_subreddits
        SET high_water_mark = $2
        WHERE name = $1",
        [Name, HighWater]
    ) of
        {ok, 1} -> ok
    end.

set_prediction(Sha, Prediction, Confidence) ->
    case pgapp:equery(
        ?POOL_NAME,
        "UPDATE images
        SET breed_prediction = $2,
        prediction_confidence = $3
        WHERE sha = $1",
        [Sha, Prediction, Confidence]
    ) of
        {ok, 1} -> ok
    end.

has_ingested_url(Url) ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT sha FROM images where original_url = $1",
        [Url]
    ) of
        {ok, _Spec, Rows} -> length(Rows) > 0
    end.

is_user_admin(User) ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT slack_id FROM administrators where slack_id = $1",
        [User]
    ) of
        {ok, _Spec, Rows} -> length(Rows) > 0
    end.

delete_image(Sha) ->
    case pgapp:equery(
        ?POOL_NAME,
        "DELETE FROM images WHERE sha = $1",
        [Sha]
    ) of
        {ok, RowsDeleted} -> RowsDeleted > 0
    end.

get_known_cat_types() ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT DISTINCT breed_prediction FROM images
         WHERE breed_prediction IS NOT NULL
         AND breed_prediction != ''",
        []
    ) of
        {ok, _Spec, Rows} ->
            [Breed || {Breed} <- Rows]
    end.

get_image_for_cat_type(Cat) ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT sha, prediction_confidence
        FROM images
        WHERE breed_prediction LIKE $1
        ORDER BY random()
        LIMIT 1",
        [Cat]
    ) of
        {ok, _Spec, []} -> not_found;
        {ok, _Spec, [{Sha, Confidence}]} -> {Sha, Confidence}
    end.

get_random_image() ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT sha, breed_prediction, prediction_confidence
        FROM images
        ORDER BY random()
        LIMIT 1",
        []
    ) of
        {ok, _Spec, [{Sha, Breed, Confidence}]} -> {Sha, Breed, Confidence}
    end.

get_stats() ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT
            count(distinct breed_prediction) as distinct_breeds,
            count(*) as total_photos,
            sum(size_bytes) as total_image_bytes
        FROM images",
        []
    ) of
        {ok, _Spec, [{Breeds, Images, Bytes}]} -> [
            {breeds, Breeds},
            {images, Images},
            {bytes, Bytes}
        ]
    end.

get_classification_stats() ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT
            CASE
                WHEN breed_prediction IS NOT NULL THEN breed_prediction
                ELSE '(uncategorized)'
            END,
            count(*) AS image_count
        FROM images
        GROUP BY breed_prediction
        ORDER BY image_count DESC",
        []
    ) of
        {ok, _Spec, Rows} -> Rows
    end.

get_all_shas() ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT sha
        FROM images",
        []
    ) of
        {ok, _RowSpec, Rows} -> [
            Sha || {Sha} <- Rows
        ]
    end.

update_sizes() ->
    % Get the image store path
    {ok, Paths} = application:get_env(catbot, paths),
    ImageDir = proplists:get_value(images, Paths),

    % Get all the SHAs we track
    Shas = get_all_shas(),

    % For each sha, check it still exists
    lists:foreach(
        fun(Sha) ->
            <<ShaPrefix:2/binary, ShaRest/binary>> = Sha,
            Path = filename:join([ImageDir, ShaPrefix, ShaRest]),
            case filelib:is_file(Path) of
                true ->
                    Size = filelib:file_size(Path),
                    pgapp:equery(
                        ?POOL_NAME,
                        "UPDATE images SET size_bytes = $1 WHERE sha = $2",
                        [Size, Sha]
                    );
                false ->
                    lager:info("Sha ~p has disappeared", [Sha]),
                    ok
            end
        end,
        Shas
    ).

vacuum_images() ->
    % Get the image store path
    {ok, Paths} = application:get_env(catbot, paths),
    ImageDir = proplists:get_value(images, Paths),

    % Get all the SHAs we track
    Shas = get_all_shas(),

    % For each sha, check it still exists
    lists:foreach(
        fun(Sha) ->
            <<ShaPrefix:2/binary, ShaRest/binary>> = Sha,
            Path = filename:join([ImageDir, ShaPrefix, ShaRest]),
            case filelib:is_file(Path) of
                true -> ok;
                false ->
                    lager:info("Sha ~p has disappeared", [Sha]),
                    ok
            end
        end,
        Shas
    ).
