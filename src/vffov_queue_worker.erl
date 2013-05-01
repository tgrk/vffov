%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Worker module that downloads videos based on specified Url queue
%%% @end
%%% Created : 29 Apr 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_queue_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/2,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {queue, current_url}).

%%%============================================================================
%%% API
%%%============================================================================
start_link(Name, Queue) ->
    gen_server:start_link({local, Name}, ?MODULE, [Queue], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Queue]) ->
    process_flag(trap_exit, true),
    {ok, #state{queue = queue:from_list(Queue), current_url = undefined}, 0}.

handle_call(Call, From, State) ->
    vffov_common:verbose(error, "Unmatched call ~p from ~p", [Call, From]),
    {reply, invalid_call, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    vffov_common:verbose(error, "Unmatched cast ~p", [Cast]),
    {noreply, State}.

handle_info(timeout, State) ->
    NewState = do_download(State),
    {noreply, NewState};
handle_info({_Port, {data, []}}, State) ->
    {noreply, State};
handle_info({_Port, {data, "\n"}}, State) ->
    {noreply, State};
handle_info({_Port, {data, Data}}, #state{current_url = Url} = State) ->
    vffov_common:handle_downloader_output(Url, Data),
    {noreply, State};
handle_info({_Port, {exit_status,1}}, #state{current_url = Url} = State) ->
    vffov_common:verbose(info, "Downloading stopped ~s", [Url]),
    {noreply, do_download(State)};
handle_info({_Port, {exit_status, 0}}, #state{current_url = Url} = State) ->
    vffov_common:verbose(info, "Finished downloading ~s", [Url]),
    {noreply, do_download(State)};
handle_info({'EXIT', _Port, normal}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    vffov_common:verbose(error, "Unmatched info ~p, ~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
do_download(#state{queue =  Queue} = State) ->
   {{value, Url}, Queue2} = queue:out(Queue),
    vffov_common:verbose(info, "Downloading video from url=~s", [Url]),
    vffov_common:open_downloader_port(Url),
    State#state{queue = Queue2, current_url = Url}.
