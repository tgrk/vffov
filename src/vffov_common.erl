%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Common code shared between all workers
%%% @end
%%% Created : 29 Aor 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_common).

-export([
         verbose/3,
         priv_dir/1,
         is_url/1,
         sanitize_urls/1,
         sanitize_url/1,
         move_to_download_dir/1,
         get_downloader/0,
         open_downloader_port/1,
         close_downloader_port/1
        ]).

%%=============================================================================
%% API
%%=============================================================================
verbose(Type, Msg, Args) ->
    case application:get_env(vffov, enable_logging, false) of
        false -> io:format(Msg ++ "\n", Args);
        true  -> lager:log(Type, Msg, Args)
    end.

priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

is_url(S) when is_list(S) ->
    string:str(S, "http://") > 0 orelse string:str("https://", S) > 0;
is_url(_S) ->
    false.

sanitize_urls(L) ->
    lists:map(fun sanitize_url/1, L).

sanitize_url(Url) ->
    [Base, Part2] = string:tokens(Url, "?"),
    case string:str(Base, "youtube.com") > 0 of
        true ->
            %% for YT videos remove all except video id parameter
            [{_K,V}] = lists:flatten(
                         lists:map(
                           fun(P) ->
                                   [K,V] = string:tokens(P, "="),
                                   case K =:= "v" of
                                       true  -> {K,V};
                                       false -> []
                                   end
                           end,
                           string:tokens(Part2, "&")
                          )
                        ),
            Base ++ "?v=" ++ V;
        false ->
            Url
    end.

move_to_download_dir(Url) ->
    [_,Id] = string:tokens(Url, "v="),
    Files = lists:filter(
               fun(F) -> string:str(F, Id) > 0 end,
               filelib:wildcard("*")
              ),
    TargetDir = application:get_env(vffov, download_dir, ""),
    lists:foreach(
      fun(File) -> file:rename(File, filename:join(TargetDir, File)) end,
      Files
     ).

open_downloader_port(Url) ->
    erlang:open_port(
      {spawn, build_downloader_command(Url)},
      [exit_status]
     ).

close_downloader_port(Port) ->
    erlang:port_close(Port).

get_downloader() ->
    application:get_env(vffov, downloader_path, "/usr/bin/clive").

%%=============================================================================
%% Internal functionality
%%=============================================================================
build_downloader_command(Url) ->
    lists:flatten(
      io_lib:format(
        "~s ~s ~s ~s",
        [get_downloader(),
         application:get_env(vffov, downloader_params, ""),
         "--filename-format '%t-%i.%s'",
         Url])
     ).
