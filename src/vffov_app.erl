%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Main VFFOV application
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_app).

-behaviour(application).

%% API
-export([process_sizes/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
start(_StartType, _StartArgs) ->
    case vffov_sup:start_link() of
        {ok, Pid} ->
            % add statsman pollers
            {ok, _} = statman_poller_sup:add_gauge(
                        fun statman_vm_metrics:get_gauges/0),
            {ok, _} = statman_poller_sup:add_counter(fun vffov_sup:get_stats/0),
            {ok, _} = statman_poller_sup:add_histogram(
                        fun ?MODULE:process_sizes/0, 6000),
            ok = statman_server:add_subscriber(statman_aggregator),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

process_sizes() ->
    %%TODO: limit only to vffov_workers
    Pids = lists:map(fun ({_, Pid, _, _}) -> Pid end,
                     supervisor:which_children(vffov_sup)),
    lists:zf(fun (P) ->
                     case total_memory(P) of
                         undefined ->
                             false;
                         Bytes ->
                             {true, {memory, Bytes}}
                     end
             end, Pids).

%%=============================================================================
%% Internal functionality
%%=============================================================================
total_memory(Pid) ->
    case {process_info(Pid, memory), process_info(Pid, binary)} of
        {A, B} when A =:= undefined orelse B =:= undefined ->
            undefined;
        {{memory, Memory}, {binary, B}} ->
            Memory + lists:sum([Size || {_, Size, _} <- B])
    end.
