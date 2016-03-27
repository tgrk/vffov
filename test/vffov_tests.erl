-module(vffov_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(ENABLE_MOCKING, false).

%%%=============================================================================
vffov_test_() ->
    {setup,
     fun() -> vffov:start() end,
     fun(_) -> vffov:stop() end,
     [
      {timeout, 100, {"Custom foobar test",   fun test_foobar/0}}
     ]
    }.

%%%=============================================================================
test_foobar() ->
    ?assert(false).

%%%============================================================================
