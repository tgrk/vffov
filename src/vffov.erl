%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% VFFOV API
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov).
-include("vffov.hrl").

%% API
-export([download/0,
         download/1,
         download/2,

         status/0,
         queue/0,
         clean_queue/0,
         stats/0,
         help/0,
         plugins/0,
         set_download_mode/1,
         set_post_download_cmd/1,

         start/0,
         stop/0
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================
-spec download() -> ok | error.
download() ->
    Opts = #{type   => file,
             args   => vffov_utils:priv_dir(vffov) ++ "playlist.txt",
             offset => -1,
             count  => -1
            },
    download(local, Opts).

-spec download(opts()) -> ok | error.
download(L) when is_list(L) ->
    Opts = #{type   => list,
             args   => L,
             offset => -1,
             count  => -1
            },
    download(local, Opts);
download(Opts) when is_map(Opts) ->
    DefaultOpts = #{type   => file,
                    args   => vffov_utils:priv_dir(vffov) ++ "playlist.txt",
                    offset => -1,
                    count  => -1
                   },
    download(local, maps:merge(DefaultOpts, Opts)).

-spec download(mode(), opts()) -> ok | error.
download(local, Opts) ->
    case filelib:is_regular(vffov_utils:get_downloader()) of
        true  ->
            Opts1 = convert_opts(Opts),
            case maps:get(type, Opts1) of
                file ->
                    case filelib:is_regular(maps:get(args, Opts1)) of
                        true  -> download_playlist(Opts1);
                        false -> error
                    end;
                list ->
                    process_url_list(Opts1)
            end;
        false ->
            vffov_utils:verbose(error, "Downloader not found! Please check "
                                "configuration.", [])
    end;
download(getpocket, PluginArgs) ->
    case vffov_getpocket:auth() of
        {ok, {request_url, Url}} ->
            vffov_utils:verbose(info, "Open following ~p in your browser and"
                                " run again.~n", [Url]);
        ok ->
            case vffov_getpocket:list(PluginArgs) of
                empty ->
                    vffov_utils:verbose(info, "No matching items.~n", []);
                {error, Reason} ->
                    vffov_utils:verbose(error,
                                        "Unable to get items from getpocket "
                                        "service! Error ~p", [Reason]);
                List  -> handle_download(lists:reverse(List))
            end;
        error ->
            vffov_utils:verbose(error,
                                "Unable to reqeust authentification code! "
                                "Check you consumer key!", [])
    end;
download(PluginName, _PluginArgs) ->
    vffov_utils:verbose(error, "Unknown plugin ~s! Existing plugins: "
                        "vffov:plugins()", [PluginName]),
    error.

-spec stats() -> [any()].
stats() ->
    simple_cache:ops_list().

-spec status() -> [any()].
status() ->
    lists:filtermap(
      fun ({_Id, Pid, worker, [vffov_parallel_worker]}) ->
              {ok, Url} = gen_server:call(Pid, current_url),
              {true, {paralel_worker, Url}};
          ({_Id, Pid, worker, [vffov_queued_worker]}) ->
              {ok, Url} = gen_server:call(Pid, current_url),
              {true, {queued_worker, Url}};
          (_) ->
              false
      end, supervisor:which_children(vffov_sup)).

-spec queue() -> queue:queue().
queue() ->
    Filtered = lists:filtermap(
                 fun ({_Id, Pid, worker, [vffov_queued_worker]}) ->
                         {ok, Queue} = gen_server:call(Pid, current_queue),
                         {true, Queue};
                     (_) ->
                         false
                 end, supervisor:which_children(vffov_sup)),
    case Filtered =:= [] of
        true  -> queue:new();
        false -> hd(Filtered)
    end.

-spec clean_queue() -> ok | no_return().
clean_queue() ->
    vffov_queued_worker:clean().

-spec help() -> ok.
help() ->
    vffov_app:print_welcome().

-spec set_download_mode(paralel | queued) -> any().
set_download_mode(parallel) ->
    application:set_env(vffov, download_parallel, true);
set_download_mode(queued) ->
    application:set_env(vffov, download_parallel, false);
set_download_mode(_Other) ->
    vffov_utils:verbose(info, "Allowed modes: parallel or queued", []).

-spec set_post_download_cmd(string()) -> ok.
set_post_download_cmd(Cmd) ->
    application:set_env(vffov, post_download_cmd, Cmd).

-spec plugins() -> list({atom(), boolean()}).
plugins() ->
    {ok, Getpocket} = application:get_env(vffov, enable_getpocket),
    {ok, Youtube} = application:get_env(vffov, enable_youtube),
    [{getpocket, Getpocket},
     {youtube,   Youtube}].

-spec start() -> ok.
start() ->
    [application:ensure_all_started(A) || A <- deps()],

    application:load(vffov),
    load_plugins(),
    application:start(vffov),
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- deps() ++ [vffov]],
    ok.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
load_plugins() ->
    Plugins = [getpocket],
    lists:foreach(
      fun (Plugin) ->
              PluginL = atom_to_list(Plugin),
              Key = list_to_atom("enable_" ++ PluginL),
              case application:get_env(vffov, Key) of
                  {ok, true} ->
                      Module = list_to_atom("vffov_" ++ PluginL),
                      Module:load();
                  _ -> ignore
              end
      end, Plugins),
    ok.

download_playlist(Opts) ->
    Input = maps:get(args, Opts),
    Opts1 = case vffov_utils:is_url(Input) of
                true  -> maps:put(args, [Input], Opts);
                false -> maps:put(args, parse(Input), Opts)
            end,
    handle_download(Opts1).

process_url_list(Opts) ->
    Input = maps:get(args, Opts),
    Opts1 = case length(Input) > 0 andalso vffov_utils:is_url(hd(Input)) of
                true  -> maps:put(args, [Input], Opts);
                false -> maps:put(args, Input, Opts)
    end,
    handle_download(Opts1).

handle_download(Opts) ->
    Offset     = maps:get(offset, Opts, 0),
    Count      = maps:get(count, Opts, 1),
    ListOfUrls = case {Offset, Count} of
                     {-1, -1} ->
                         %% all
                         maps:get(args, Opts, []);
                     _ ->
                         %% offset
                         lists:sublist(maps:get(args, Opts), Offset, Count)
                 end,

    Sanitized = vffov_utils:sanitize_urls(ListOfUrls),
    case application:get_env(vffov, download_parallel, false) of
        true  -> lists:foreach(fun download_file/1, Sanitized);
        false -> queue_downloads(Sanitized)
    end.

download_file({[{<<"url">>, Url}]}) ->
    vffov_sup:start_worker(vffov_parallel_worker, Url);
download_file(Url) ->
    vffov_sup:start_worker(vffov_parallel_worker, Url).

queue_downloads(List) ->
    vffov_sup:start_worker(vffov_queued_worker, List).

parse(Path) ->
    try
        {ok, Bin} = file:read_file(Path),
        case filename:extension(Path) of
            ".json" -> parse_1(json, Bin);
            ".txt"  -> parse_1(txt, Bin);
            Other   -> throw({unknown_file_extension, Other})
        end
    catch
        _:Reason ->
            vffov_utils:verbose(error, "Unable to load playlist file! Error ~p",
                                [Reason])
    end.

parse_1(json, Bin) ->
    case jiffy:decode(Bin) of
        {[{<<"list">>, List}]} -> List;
        _ -> empty_playlist
    end;
parse_1(txt, Bin) ->
    case string:tokens(erlang:binary_to_list(Bin), "\n") of
        []   -> empty_playlist;
        List -> List
    end.

convert_opts(M) when is_map(M) ->
    M;
convert_opts(PL) when is_list(PL) ->
    maps:from_list(PL).

deps() ->
    [lager, jiffy, statman, simple_cache].
