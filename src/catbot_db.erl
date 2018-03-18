-module(catbot_db).
-compile(export_all).

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
