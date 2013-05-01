%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Main VFFOV supervisor
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/3]).

%% Supervisor callbacks
-export([init/1]).

%%=============================================================================
%% API functions
%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Type, Name, Url) ->
    case supervisor:start_child(?MODULE, get_child(Type, Name, Url)) of
        {error, Reason} ->
            throw({unable_to_start_worker, Type, Name, Reason});
        {ok, Pid} ->
            Pid
    end.

%%=============================================================================
%% Supervisor callbacks
%%=============================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
get_child(parallel, Name, Arg) ->
    WorkerName = get_worker_name(Name),
    vffov_common:verbose(info, "Starting parallel worker: ~p", [WorkerName]),
    {WorkerName, {vffov_worker, start_link, [WorkerName, Arg]},
     temporary, brutal_kill, worker, [vffov_worker]};
get_child(queued, Name, Arg) ->
    WorkerName = get_worker_name(Name),
    vffov_common:verbose(info, "Starting queued worker: ~p", [WorkerName]),
    {WorkerName, {vffov_queue_worker, start_link, [WorkerName, Arg]},
     temporary, brutal_kill, worker, [vffov_queue_worker]}.

get_worker_name(Name) ->
    list_to_atom("vffov_queue_worker_" ++ Name).
