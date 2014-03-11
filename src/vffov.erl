%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% VFFOV API
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov).

%% API
-export([download/0,
         download/1,
         download_pocket/1,

         status/0,
         queue/0,

         start/0,
         stop/0
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================
download() ->
    download(vffov_common:priv_dir(vffov) ++ "playlist.txt").

download(L) when is_list(L) ->
    case filelib:is_regular(vffov_common:get_downloader()) of
        true  ->
            case filelib:is_regular(L) of
                true  -> download_1(L);
                false -> process_url_list(L)
            end;
        false ->
            vffov_common:verbose(error, "Downloader not found! Please check "
                                 "configuration.", [])
    end.

download_pocket(Opts) ->
    case vffov_getpocket:auth() of
        {ok, {request_url, Url}} ->
            vffov_common:verbose(info, "Open following ~p in your browser and"
                                 " run again.~n", [Url]);
        ok ->
            case vffov_getpocket:list(Opts) of
                empty ->
                    vffov_common:verbose(info, "No matching items.~n", []);
                {error, Reason} ->
                    vffov_common:verbose(error,
                                         "Unable to get items from getpocket "
                                         "service! Error ~p", [Reason]);
                List  -> handle_download(List)
            end;
        error ->
            vffov_common:verbose(error,
                                 "Unable to reqeust authentification code! "
                                 "Check you consumer key!", [])
    end.


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

queue() ->
    lists:filtermap(
      fun ({_Id, Pid, worker, [vffov_queued_worker]}) ->
              {ok, Queue} = gen_server:call(Pid, current_queue),
              {true, Queue};
          (_) ->
              false
      end, supervisor:which_children(vffov_sup)).

start() ->
    [application:start(A) || A <- deps()],
    application:load(vffov),

    load_plugins(),

    application:start(vffov).

stop() ->
    [application:stop(A) || A <- deps() ++ [vffov]].

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

download_1(Input) ->
    case vffov_common:is_url(Input) of
        true  ->
            %% handle input form console
            handle_download([Input]);
        false ->
            %% handle file input
            handle_download(parse(Input))
    end.

process_url_list(L) when is_list(L) ->
    case length(L) > 0 andalso vffov_common:is_url(hd(L)) of
        true  -> handle_download(L);
        false -> handle_download([L])
    end.

handle_download(List) ->
    Sanitized = vffov_common:sanitize_urls(List),
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
            vffov_common:verbose(error,
                                 "Unable to load playlist file! Error ~p",
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

deps() ->
    [compiler, syntax_tools, lager, jiffy, statman].
