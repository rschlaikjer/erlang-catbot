-module(catbot_image_sink).
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
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ingest(Url) ->
    gen_server:cast(?MODULE, {ingest_url, Url}).

init([]) ->
    {ok, #state{}}.

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
