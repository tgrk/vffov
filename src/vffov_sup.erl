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
-export([get_stats/0]).

%% Supervisor callbacks
-export([init/1]).

%%=============================================================================
%% API functions
%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Worker, Url) ->
    case supervisor:start_child(?MODULE, get_child(Worker, Url)) of
        {error, Reason} ->
            throw({unable_to_start_worker, Worker, Reason});
        {ok, Pid} ->
            Pid
    end.

get_stats() ->
    StatsPL = supervisor:count_children(?MODULE),
    [
     {{vffov, workers}, proplists:get_value(workers, StatsPL)}
    ].

%%=============================================================================
%% Supervisor callbacks
%%=============================================================================
init([]) ->
    ChildSpecs = [
                  statman_aggregator(),
                  statman_elli(),
                  elli()
                 ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
get_child(Worker, Arg) ->
    Name = get_worker_name(Worker),
    {Name, {Worker, start_link, [Name, Arg]}, temporary, brutal_kill,
     worker, [Worker]}.

get_worker_name(Worker) ->
    {_, _, Mics} = erlang:now(),
    list_to_atom(atom_to_list(Worker) ++ "_" ++ integer_to_list(Mics)).

%% Childspecs required by statman dashboard
statman_aggregator() ->
    {statman_aggregator, {statman_aggregator, start_link, []},
     permanent, 5000, worker, []}.

statman_elli() ->
    {statman_elli, {statman_elli_server, start_link, []},
     permanent, 5000, worker, []}.

elli() ->
    Opts = [{callback, elli_middleware},
            {callback_args, [{mods, get_elli_mods()}]},
            {port, application:get_env(vffov, api_port, 8081)}
           ],
    {elli, {elli, start_link, [Opts]}, permanent, 5000, worker, []}.

get_elli_mods() ->
    {ok, Cwd} = file:get_cwd(),
    StatmanConfig = [{name, collect_statman_elli},
                     {docroot, filename:join(
                                 [Cwd, "deps/statman_elli/priv/docroot"]
                                )
                     }],
    DefaultMods = [{statman_elli, StatmanConfig},
                   {elli_access_log, []}],

    ApiMod = case application:get_env(vffov, enable_api) of
                 {ok, true}  -> [{vffov_api, []}];
                 {ok, false} -> []
             end,
    [DefaultMods | ApiMod].
