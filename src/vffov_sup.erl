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
-export([start_link/0, start_worker/2]).

%% Supervisor callbacks
-export([init/1]).

%%=============================================================================
%% API functions
%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Name, Url) ->
    case supervisor:start_child(?MODULE, get_child(Name, Url)) of
        {error, Reason} ->
            throw({unable_to_start_worker, Name, Reason});
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

get_child(Name, Url) ->
    WorkerName = list_to_atom("vffov_worker_" ++ Name),
    vffov:verbose(info, "Starting worker: ~p", [WorkerName]),
    {WorkerName, {vffov_worker, start_link, [WorkerName, Url]},
     temporary, brutal_kill, worker, [vffov_worker]}.
