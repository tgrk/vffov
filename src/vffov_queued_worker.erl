%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Worker module that downloads videos based on specified Url queue
%%% @end
%%% Created : 29 Apr 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_queued_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         stop/0,
         enqueue/1,
         get_url/0,
         get_queue/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port, queue, id = undefined, current_url = undefined}).

%%%============================================================================
%%% API
%%%============================================================================
%%TODO: type spec
start_link(Queue) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Queue], []).

stop() ->
    gen_server:cast(?MODULE, stop).

enqueue(Url) ->
    gen_server:cast(?MODULE, {enqueue, Url}).

get_url() ->
    gen_server:call(?MODULE, current_url).

get_queue() ->
    gen_server:call(?MODULE, current_queue).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Queue]) ->
    process_flag(trap_exit, false),
    {ok, #state{queue = queue:from_list(Queue)}, 0};
init([]) ->
    process_flag(trap_exit, false),
    {ok, #state{queue = queue:new()}, 0}.

handle_call(current_url, _From, State) ->
    {reply, {ok, State#state.current_url}, State};
handle_call(current_queue, _From, State) ->
    {reply, {ok, State#state.queue}, State};
handle_call(Call, From, State) ->
    vffov_utils:verbose(error, "Unmatched call ~p from ~p", [Call, From]),
    {reply, invalid_call, State}.

handle_cast({enqueue, Url}, #state{queue = Queue} = State) ->
    vffov_utils:verbose(info, "Add ~s to queue", [Url]),
    {noreply, State#state{queue = queue:in(Url, Queue)}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    vffov_utils:verbose(error, "Unmatched cast ~p", [Cast]),
    {noreply, State}.

handle_info(timeout, State) ->
    do_download(State);
handle_info({_Port, {data, Data}}, #state{current_url = Url} = State) ->
    vffov_utils:verbose(info, "~s - ~s", [Url, Data]),
    {noreply, State};
handle_info({_Port, {exit_status,1}}, #state{current_url = Url} = State) ->
    vffov_utils:verbose(info, "Downloading stopped ~s", [Url]),
    do_download(State);
handle_info({_Port, {exit_status, 0}}, #state{id = Id, current_url = Url}
            = State) ->
    vffov_utils:verbose(info, "Finished downloading ~s (id=~p)", [Url, Id]),
    vffov_utils:move_to_download_dir(Url),

    %% mark as downloaded (getpocket)
    case Id =/= undefined of
        true  -> vffov_getpocket:mark_done(Id);
        false -> ignore
    end,
    do_download(State);
handle_info({'EXIT', _Port, normal}, #state{queue = []} = State) ->
    {stop, normal, State};
handle_info({'EXIT', _Port, normal}, State) ->
    do_download(State);
handle_info(Info, State) ->
    vffov_utils:verbose(error, "Unmatched info ~p, ~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
do_download(#state{queue = Queue} = State) ->
   case queue:out(Queue) of
      {{value, {Id, Url}}, Queue2} ->
           vffov_utils:verbose(info, "Downloading video from url ~s", [Url]),
           Port = vffov_utils:open_downloader_port(Url),
           {noreply,
            State#state{port = Port, queue = Queue2, id = Id, current_url = Url}
           };
       {{value, Url}, Queue2} ->
           vffov_utils:verbose(info, "Downloading video from url ~s", [Url]),
           Port = vffov_utils:open_downloader_port(Url),
           {noreply,
            State#state{port = Port, queue = Queue2, current_url = Url}
           };
       _ ->
           {stop, normal, State#state{queue = [], current_url = []}}
   end.
