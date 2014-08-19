%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Main VFFOV application
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
-spec start(_,_) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_StartType, _StartArgs) ->
    case vffov_sup:start_link() of
        {ok, Pid} ->
            % add statsman pollers
            {ok, _} = statman_poller_sup:add_gauge(
                        fun statman_vm_metrics:get_gauges/0),
            {ok, _} = statman_poller_sup:add_counter(fun vffov_sup:get_stats/0),
            {ok, _} = statman_poller_sup:add_histogram(
                        fun vffov_utils:process_sizes/0, 6000),
            ok = statman_server:add_subscriber(statman_aggregator),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(any()) -> 'ok'.
stop(_State) ->
    ok.
