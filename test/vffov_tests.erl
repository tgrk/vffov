-module(vffov_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
vffov_test_() ->
    {setup,
     fun() -> vffov:start() end,
     fun(_) -> vffov:stop() end,
     [
      {timeout, 100, {"Custom API test",   fun test_api/0}}
     ]
    }.

%%%=============================================================================
test_api() ->
    ?assert(false).

%%%============================================================================
