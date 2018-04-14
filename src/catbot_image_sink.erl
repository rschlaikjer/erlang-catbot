-module(catbot_image_sink).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-define(WORKER_COUNT, 8).

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

-record(state, {
    pending_urls = [],
    workers = [],
    free_workers = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ingest(Url, AutoPrediction) when is_binary(Url) ->
    ingest(binary_to_list(Url), AutoPrediction);
ingest(Url, AutoPrediction) when is_list(Url) ->
    gen_server:cast(?MODULE, {ingest_url, Url, AutoPrediction}).

init([]) ->
    process_flag(trap_exit, true),
    Workers = init_worker_pool(),
    {ok, #state{
        workers=Workers,
        free_workers=Workers
    }}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast({ingest_url, Url, AutoPrediction}, State) ->
    State1 = ingest_url(State, Url, AutoPrediction),
    {noreply, State1};
handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info({ingest_ok, WorkerPid}, State) ->
    State1 = handle_worker_return(State, WorkerPid),
    {noreply, State1};
handle_info({'EXIT', Pid, _Reason}, State) ->
    State1 = handle_worker_death(State, Pid),
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

init_worker_pool() ->
    Results = [
        catbot_image_sink_worker:start_link() ||
        _ <- lists:seq(1, ?WORKER_COUNT)
    ],
    _Pids = [Pid || {ok, Pid} <- Results].

handle_worker_return(State, Pid) ->
    case State#state.pending_urls of
        [] ->
            % No pending work, put worker back on ready list
            State#state{free_workers=[Pid|State#state.free_workers]};
        [{Url, Prediction}|Urls] ->
            % Pending url, send it to the worker and leave off the ready list
            catbot_image_sink_worker:ingest(Pid, Url, Prediction),
            State#state{pending_urls=Urls}
    end.

handle_worker_death(State, DeadPid) ->
    % Remove the dead pid from worker + free worker lists
    Workers1 = [Pid || Pid <- State#state.workers, Pid  =/= DeadPid],

    % Add a new worker
    {ok, NewWorker} = catbot_image_sink_worker:start_link(),
    lager:info("Replacing dead worker ~p with new worker ~p", [DeadPid, NewWorker]),

    % Pretend the worker just finished a task so it gets picked up
    self() ! {ingest_ok, NewWorker},

    % Log this as an error somewhere
    estatsd:increment("catbot.ingest.error"),
    State#state{free_workers=Workers1}.

ingest_url(State, Url, AutoPrediction) ->
    case has_free_worker(State) of
        true ->
            {ok, State1, Pid} = checkout_worker(State),
            catbot_image_sink_worker:ingest(Pid, Url, AutoPrediction),
            State1;
        false ->
            State#state{
                pending_urls=[{Url, AutoPrediction}|State#state.pending_urls]
            }
    end.

has_free_worker(State) ->
    case State#state.free_workers of
        [] -> false;
        _ -> true
    end.

checkout_worker(State) ->
    [W|Workers] = State#state.free_workers,
    {ok, State#state{free_workers=Workers}, W}.
