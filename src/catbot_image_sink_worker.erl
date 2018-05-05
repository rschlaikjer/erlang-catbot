-module(catbot_image_sink_worker).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-export([
    start_link/0,
    ingest/3
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

ingest(Pid, Url, AutoPrediction) when is_list(Url) ->
    gen_server:cast(Pid, {ingest_url, self(), Url, AutoPrediction}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast({ingest_url, From, Url, AutoPrediction}, State) ->
    ingest_url(State, From, Url, AutoPrediction),
    {noreply, State};
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

ingest_url(State, From, Url, AutoPrediction) ->
    case catbot_db:has_ingested_url(Url) of
        true ->
            % Don't need to re-process
            ok;
        false ->
            case is_image_url(Url) of
                false ->
                    % Only care about images
                    ok;
                true ->
                    case download_image(Url) of
                        {ok, ImageData} ->
                            case save_image(ImageData) of
                                {ok, Sha} ->
                                    catbot_db:ingest_image(Sha, Url, byte_size(ImageData)),
                                    estatsd:increment("catbot.ingest.success"),
                                    % If we have an autoprediction based on the source, apply
                                    % that too
                                    case AutoPrediction of
                                        B when is_binary(B) ->
                                            catbot_db:set_prediction(Sha, AutoPrediction, 0.95);
                                        _ -> ok
                                    end;
                                {error, Sha, Reason} ->
                                    lager:error("Failed to save image with sha ~s: ~p", [Sha, Reason])
                            end;
                        {error, Reason} ->
                            lager:info("Failed to download image: ~p~n", [Reason]),
                            ok;
                        error ->
                            ok
                    end
            end
    end,
    From ! {ingest_ok, self()}.

save_image(ImageData) ->
    Sha1 = hash_binary(ImageData),
    ok = filelib:ensure_dir(output_path()),
    OutFile = filename:join(output_path(), Sha1),
    case filelib:is_file(OutFile) of
        true -> {ok, Sha1};
        false ->
            case file:write_file(OutFile, ImageData) of
                ok -> {ok, Sha1};
                {error, Reason} -> {error, Sha1, Reason}
            end
    end.

hash_binary(Binary) ->
    Ctx = crypto:hash_init(sha),
    Ctx1 = crypto:hash_update(Ctx, Binary),
    Digest = crypto:hash_final(Ctx1),
    << << (list_to_binary(io_lib:format("~2.16.0b", [C])))/binary >>
            || <<C>> <= Digest >>.

output_path() ->
    {ok, Paths} = application:get_env(catbot, paths),
    proplists:get_value(images, Paths).

is_image_url(Url) ->
    case httpc:request(head, {Url, []}, [{timeout, 30000}], []) of
        {error, Reason} ->
            lager:info("Failed to HEAD url ~s: ~p", [Reason]),
            false;
        {ok, {{_, 200, _}, Headers, _}} ->
            ContentType = proplists:get_value("content-type", Headers),
            is_content_type_image(ContentType);
        _ -> false
    end.

download_image(Url) ->
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {error, Reason} ->
            {error, Reason};
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        _ -> error
    end.

is_content_type_image(Type) when is_list(Type) ->
    is_content_type_image(list_to_binary(Type));
is_content_type_image(<<"image/", _Rest/binary>>) -> true;
is_content_type_image(_) -> false.
