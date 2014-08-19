%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Worker module that downloads videos based on specified Url
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_parallel_worker).
-include("vffov.hrl").
-behaviour(gen_server).

%% API
-export([
         start_link/2,
         stop/0,
         get_url/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id          :: string(),
                current_url :: url()
               }).

%%%============================================================================
%%% API
%%%============================================================================
-spec start_link(atom(), url()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Name, Url) ->
    gen_server:start_link({local, Name}, ?MODULE, [Url], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec get_url() -> url().
get_url() ->
    gen_server:call(?MODULE, current_url).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([{Id, Url}]) ->
    process_flag(trap_exit, true),
    {ok, #state{id = Id, current_url = Url}, 0};
init([Url]) ->
    process_flag(trap_exit, true),
    {ok, #state{id = undefined, current_url = Url}, 0}.

handle_call(current_url, _From, State) ->
    {reply, {ok, State#state.current_url}, State};
handle_call(Call, From, State) ->
    vffov_utils:verbose(error, "Unmatched call ~p from ~p", [Call, From]),
    {reply, invalid_call, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    vffov_utils:verbose(error, "Unmatched cast ~p", [Cast]),
    {noreply, State}.
handle_info(timeout, #state{current_url = Url} = State) ->
    vffov_utils:verbose(info, "Downloading video from url=~s", [Url]),
    vffov_utils:open_downloader_port(Url),
    {noreply, State};
handle_info({_Port, {data, Data}}, #state{current_url = Url} = State) ->
    vffov_utils:verbose(info, "~s - ~s", [Url, Data]),
    {noreply, State};
handle_info({_Port, {exit_status,1}}, #state{current_url = Url} = State) ->
    vffov_utils:verbose(info, "Downloading stopped ~s", [Url]),
    {stop, normal, State};
handle_info({_Port, {exit_status,23}}, State) ->
    vffov_utils:verbose(info, "Unable to download file! No space left.", []),
    {stop, normal, State};
handle_info({_Port, {exit_status, 0}}, #state{id = Id, current_url = Url}
            = State) ->
    vffov_utils:verbose(info, "Finished downloading ~s", [Url]),
    vffov_utils:move_to_download_dir(Url),

    %% mark as downloaded (getpocket)
    case Id =/= undefined of
        true  -> vffov_getpocket:mark_done(Id);
        false -> ignore
    end,
    {stop, normal, State};
handle_info({'EXIT', _Port, normal}, State) ->
    {stop, normal, State};
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
