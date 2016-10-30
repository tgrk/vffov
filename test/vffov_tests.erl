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
       {"Is valid URL",               fun is_url_test/0}
     , {"URL sanitization",           fun sanitize_url_test/0}
     , {"Find file by pattern",       fun filter_file_by_pattern_test/0}
     , {"Remove entry from playlist", fun remove_from_playlist_test/0}
     ]
    }.

%%%=============================================================================
is_url_test() ->
    ?assertEqual(true, vffov_utils:is_url("http://google.com")),
    ?assertEqual(false, vffov_utils:is_url("youtube.com")),
    ?assertEqual(false, vffov_utils:is_url(foo)),
    ok.

sanitize_url_test() ->
    Id = foo,
    Cases = [{"http://youtube.com/", "http://youtube.com/"},
             {"https://www.youtube.com/watch?v=eU74nqRTXjc",
              "https://www.youtube.com/watch?v=eU74nqRTXjc&index=99&list=WL"
             },
             {"https://www.google.com/", "https://www.google.com/"}
            ],

    lists:foreach(
      fun ({Sanitized, InputUrl}) ->
              ?assertEqual(
                 {Id, Sanitized}, vffov_utils:sanitize_url({Id, InputUrl}))
      end, Cases),
    ok.

filter_file_by_pattern_test() ->
    ?assertEqual("rebar.config", vffov_utils:filter_filename_by_id("rebar.config")),
    ?assertEqual([], vffov_utils:filter_filename_by_id("fooo")),
    ok.

remove_from_playlist_test() ->
    FileName = "playlist.txt",
    FullPath = filename:join("/tmp/", FileName),
    NumberOfFiles = 10,

    CreateUrl = fun (V) ->
                        "https://www.youtube.com/watch?v=" ++ integer_to_list(V)
                end,

    %% create test playlist file
    Url = lists:map(fun (V) -> CreateUrl(V) end, lists:seq(1, NumberOfFiles)),
    ok = file:write_file(FullPath, string:join(Url, "\n"), [write]),

    %% mock priv_dir to use tmp dir
    ok   = meck:new(vffov_utils, [passthrough]),
    true = meck:validate(vffov_utils),
    ok   = meck:expect(vffov_utils, priv_dir, 1, "/tmp/"),

    %% remove random url from list
    RandomUrl = CreateUrl(random:uniform(NumberOfFiles)),
    ?assertEqual(ok, vffov_utils:maybe_remove_from_playlist_file(RandomUrl)),

    ok =  meck:unload(vffov_utils),

    %% verify that random file from list is not present
    Results = vffov_utils:readlines(FullPath),
    ?assertNotEqual(NumberOfFiles, length(Results)),

    ok.

%%%============================================================================
