%% Globals
-define(APP, vffov).

%% Types
-type workers() :: vffov_parallel_worker | vffov_queued_worker.
-type mode()    :: atom() | local | list | file.
-type opts()    :: map() | string() | list(string()).
