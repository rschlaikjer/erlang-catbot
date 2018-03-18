-module(catbot_image_sink_worker).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-export([
    start_link/0,
    ingest/2
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

ingest(Pid, Url) when is_list(Url) ->
    gen_server:cast(Pid, {ingest_url, self(), Url}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast({ingest_url, From, Url}, State) ->
    ingest_url(State, From, Url),
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

ingest_url(State, From, Url) ->
    case is_image_url(Url) of
        true ->
            case download_image(Url) of
                {ok, ImageData} ->
                    {ok, FilePath} = save_image(ImageData),
                    catbot_db:ingest_image(Url, FilePath);
                error ->
                    ok
            end;
        false -> ok
    end,
    lager:info("Ingested url ~s", [Url]),
    From ! {ingest_ok, self()}.

save_image(ImageData) ->
    Sha1 = hash_binary(ImageData),
    ok = filelib:ensure_dir(output_path()),
    OutFile = filename:join(output_path(), Sha1),
    ok = file:write_file(OutFile, ImageData),
    {ok, OutFile}.

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
    case httpc:request(head, {Url, []}, [], []) of
        {ok, {{_, 200, _}, Headers, _}} ->
            ContentType = proplists:get_value("content-type", Headers),
            is_content_type_image(ContentType);
        _ -> false
    end.

download_image(Url) ->
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        _ -> error
    end.

is_content_type_image(Type) when is_list(Type) ->
    is_content_type_image(list_to_binary(Type));
is_content_type_image(<<"image/", _Rest/binary>>) -> true;
is_content_type_image(_) -> false.
