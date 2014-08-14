%% Globals
-define(APP, vffov).

%% Types
-type workers() :: vffov_parallel_worker | vffov_queued_worker.
-type url()     :: string() | list(url()).
