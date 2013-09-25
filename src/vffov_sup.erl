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
    ChildSpecs = [
                  statman(),
                  statman_aggregator(),
                  statman_poller(),
                  statman_elli(),
                  elli()
                 ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
get_child(parallel = Type, Name, Arg) ->
    WorkerName = get_worker_name(Type, Name),
    vffov_common:verbose(info, "Starting parallel worker: ~p", [WorkerName]),
    {WorkerName, {vffov_worker, start_link, [WorkerName, Arg]},
     temporary, brutal_kill, worker, [vffov_worker]};
get_child(queued = Type, Name, Arg) ->
    WorkerName = get_worker_name(Type, Name),
    vffov_common:verbose(info, "Starting queued worker: ~p", [WorkerName]),
    {WorkerName, {vffov_queue_worker, start_link, [WorkerName, Arg]},
     temporary, brutal_kill, worker, [vffov_queue_worker]}.

get_worker_name(queued, Name) ->
    list_to_atom("vffov_queue_worker_" ++ Name);
get_worker_name(parallel, Name) ->
    list_to_atom("vffov_worker_" ++ Name).

%% Childspecs required by statman dashboard
statman() ->
    {statman, {statman_server, start_link, [1000]},
     permanent, 5000, worker, []}.

statman_aggregator() ->
    {statman_aggregator, {statman_aggregator, start_link, []},
     permanent, 5000, worker, []}.

statman_poller() ->
    {statman_gauge_poller, {statman_poller, start_link, []},
     permanent, 5000, worker, []}.

statman_elli() ->
    {statman_elli, {statman_elli_server, start_link, []},
     permanent, 5000, worker, []}.

elli() ->
    {ok, Cwd} = file:get_cwd(),
    StatmanConfig = [{name, collect_statman_elli},
                     {docroot, filename:join(
                                 [Cwd, "deps/statman_elli/priv/docroot"]
                                )
                     }
                    ],

    Middleware = [{mods, [
                          {statman_elli, StatmanConfig},
                          {elli_access_log, []},
                          {collect_options, []},
                          {collect_health_check, []},
                          {collect_crossdomain, []},
                          {collect_api, []}
                         ]}
                 ],

    Opts = [{callback, elli_middleware},
            {callback_args, Middleware},
            {port, 8080}
           ],
    {elli, {elli, start_link, [Opts]}, permanent, 5000, worker, []}.
