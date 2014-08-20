-module(vffov_api_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(ENABLE_MOCKING, false).

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
http_get(Path) ->
    http_request(get, Path, []).

http_post(Path, Body) ->
    http_request(post, Path, Body).

http_delete(Path) ->
    http_request(delete, Path, []).

http_request(Method, Path, Body) ->
    http_request(Method, Path, Body, json).

http_request(Method, Path, Body, ContentType) ->
    URL = "http://localhost:8081" ++ Path,
    lhttpc:request(URL, Method, get_http_header(ContentType), Body, 60000).

get_http_header(form) ->
    [{"Content-Type", "application/x-www-form-urlencoded"}];
get_http_header(json) ->
    [{"Content-Type", "application/json"}];
get_http_header(_) ->
    [].

http_status_code({{Code, _}, _, _}) -> Code.
http_body({_, _, Body})             -> Body.
http_headers({_, Headers, _})       -> Headers.
