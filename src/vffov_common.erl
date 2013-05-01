%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Common code shared between all workers
%%% @end
%%% Created : 29 Aor 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_common).


-compile(export_all).

%%=============================================================================
%% API
%%=============================================================================
verbose(Type, Msg, Args) ->
    case application:get_env(vffov, enable_logging, false) of
        false -> io:format(Msg ++ "\n", Args);
        true  -> lager:log(Type, Msg, Args)
    end.

open_downloader_port(Url) ->
    Cmd = build_downloader_command(Url),
    erlang:open_port({spawn, Cmd}, [exit_status]).

handle_downloader_output(Url, Data) ->
    case string:substr(Data, 1, 11) == "\r[download]" of
        true  ->
            verbose(info, "Downloading ~s - ~s",
                          [Url, string:sub_string(Data, 15)]);
        false ->
            verbose(info, "~s - ~s", [Url, Data])
    end.

remove_https(Url) ->
    case string:str(Url, "https://") of
        0 -> Url;
        _ -> re:replace(Url, "https", "http", [{return, list}])
    end.

%%=============================================================================
%% Internal functionality
%%=============================================================================
build_downloader_command(Url) ->
    DownloaderPath = application:get_env(vffov, downloader_path, "youtube-dl"),
    DownloaderTemplate = case application:get_env(vffov, download_dir, "./") of
                             "./" -> [];
                             ""   -> [];
                             Path ->
                                 ["-o '" ++ Path ++ "%(title)s-%(id)s.%(ext)s'"]
                         end,
    DownloaderParams = application:get_env(vffov, downloader_params, ""),
    lists:flatten(
      io_lib:format("~s ~s ~s ~s",
                    [DownloaderPath, DownloaderTemplate, DownloaderParams, Url])
     ).
