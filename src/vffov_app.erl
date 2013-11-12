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
start(_StartType, _StartArgs) ->
    case vffov_sup:start_link() of
        {ok, Pid} ->
            % add statsman
            {ok, _} = statman_poller_sup:add_gauge(fun statman_vm_metrics:get_gauges/0),
            ok = statman_server:add_subscriber(statman_aggregator),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
vffov_gauges() ->
    case catch s3:stats() of
        {ok, Stats} ->
            Workers = proplists:get_value(num_workers, Stats),
            [{{s3, workers}, Workers}];
        _ ->
            []
    end.
