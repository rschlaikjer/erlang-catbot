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
              {size, 4},
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

ingest_image(OriginalUrl, FilePath) ->
    case pgapp:equery(
        ?POOL_NAME,
        "INSERT INTO images
        (original_url, path)
        VALUES
        ($1, $2)",
        [OriginalUrl, FilePath]
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

set_prediction(ImagePath, Prediction, Confidence) ->
    case pgapp:equery(
        ?POOL_NAME,
        "UPDATE images
        SET breed_prediction = $2,
        prediction_confidence = $3
        WHERE path = $1",
        [ImagePath, Prediction, Confidence]
    ) of
        {ok, 1} -> ok
    end.

has_ingested_url(Url) ->
    case pgapp:equery(
        ?POOL_NAME,
        "SELECT path FROM images where original_url = $1",
        [Url]
    ) of
        {ok, _Spec, Rows} -> length(Rows) > 0
    end.