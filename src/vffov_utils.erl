%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Common code shared between all workers
%%% @end
%%% Created : 29 Apr 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_utils).

%% API
-export([ verbose/3
        , priv_dir/1
        , is_url/1
        , sanitize_urls/1
        , sanitize_url/1
        , move_to_download_dir/2
        , maybe_execute_command/2
        , download_finished/2
        , get_downloader/0
        , open_downloader_port/1
        , close_downloader_port/1
        , read_pocket_credentials/0
        , write_pocket_credentials/3

          %% Exported for testing only
        , filter_filename_by_id/1
        , maybe_remove_from_playlist_file/1
        , readlines/1
        ]).

%%=============================================================================
%% API
%%=============================================================================
-spec verbose(atom(), string(), list()) -> ok.
verbose(Type, Msg, Args) ->
    case application:get_env(vffov, enable_logging, false) of
        false -> io:format(Msg ++ "\n", Args);
        true  -> lager:log(Type, Msg, Args)
    end,

    case application:get_env(vffov, enable_api, false) of
        false -> ignore;
        true  ->
            vffov_notify_server:add_notification(
              <<(atom_to_binary(Type, latin1))/binary, <<": ">>/binary,
                (list_to_binary(io_lib:format(Msg, Args)))/binary>>
             )
    end,
    ok.

-spec priv_dir(atom()) -> string().
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

-spec is_url(string()) -> boolean().
is_url(S) when is_list(S) ->
    string:str(S, "http://") > 0 orelse string:str("https://", S) > 0;
is_url(_S) ->
    false.

-spec sanitize_urls(list(string())) -> list(string()).
sanitize_urls(L) ->
    lists:map(fun sanitize_url/1, L).

-spec sanitize_url(string() | {string(),string()}) -> list(string()).
sanitize_url({Id, Url}) ->
    {Id, sanitize_url(Url)};
sanitize_url(Url) ->
    case string:tokens(Url, "?") of
        [_] ->
            Url;
        [Base, Part2] ->
            case string:str(Base, "youtube.com") > 0 of
                true ->
                    %% for YT videos remove all except video id parameter
                    [V] = lists:filtermap(
                            fun(P) ->
                                    case string:tokens(P, "=") of
                                        ["v", Val] -> {true, Val};
                                        _           -> false
                                    end
                            end,
                            string:tokens(Part2, "&")
                           ),
                    Base ++ "?v=" ++ V;
                false ->
                    Url
            end
    end.

-spec move_to_download_dir(string(), pos_integer()) -> {ok, string()} | no_return().
move_to_download_dir(Url, Start) ->
    %% find file by id in path (note that this is YT specific)
    [_ | Id]  = string:tokens(Url, "="),
    File      = filter_filename_by_id(Id),
    TargetDir = application:get_env(vffov, download_dir, ""),

    {ok, FileInfo} = file:read_file_info(File),

    %% update file stats
    simple_cache:set(erlang:phash2(Url),
                     [{file,   File},
                      {url,    Url},
                      {status, finished},
                      {size,   element(2, FileInfo)},
                      {start,  edatetime:ts2datetime(Start)},
                      {finish, element(5, FileInfo)}
                     ]),

    %% create valid absolute paths
    TargetPath = filename:join(TargetDir, File),
    {ok, SourceDir} = file:get_cwd(),
    SourcePath = filename:join(SourceDir, File),

    %% finally move file
    ok = file:rename(SourcePath, TargetPath),

    {ok, TargetPath}.

-spec download_finished(string(), any()) -> ok | no_return().
download_finished(Url, Id) ->
    %%FIXME: handle plugins generically
    %% mark as downloaded (getpocket)
    case Id =/= undefined of
        true  -> vffov_getpocket:mark_done(Id);
        false -> maybe_remove_from_playlist_file(Url)
    end,
    ok.

%%TODO: we are ignoring path argument here!
-spec maybe_execute_command(atom(), string()) -> ok.
maybe_execute_command(post, _Path) ->
    try
        case application:get_env(vffov, post_download_cmd, undefined) of
            undefined -> ok;
            ""        -> ok;
            Command   ->
                os:cmd(Command),
                ok
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec open_downloader_port(string()) -> port().
open_downloader_port(Url) ->
    %% add file stats
    simple_cache:set(erlang:phash2(Url),
                     [{file,   ""},
                      {url,    Url},
                      {status, downloading},
                      {size,   0},
                      {start,  edatetime:now2ts()},
                      {finish, 0}
                     ]),

    Path = build_downloader_command(Url),
    verbose(info, "Downloader path=~p", [Path]),
    erlang:open_port({spawn, Path}, [exit_status]).

-spec close_downloader_port(atom() | port()) -> true.
close_downloader_port(Port) ->
    erlang:port_close(Port).

-spec get_downloader() -> {ok, string()} | error.
get_downloader() ->
    Path = application:get_env(vffov, downloader_path, "/usr/bin/youtube-dl"),
    case filelib:is_regular(Path) of
        true  -> {ok, Path};
        false -> error
    end.

-spec write_pocket_credentials(string(), string(), string())
                              -> ok | {error, term()}.
write_pocket_credentials(Code, ConsumerKey, AccessToken) ->
    Data = [{code,         Code},
            {consumer_key, ConsumerKey},
            {access_token, AccessToken}],
    file:write_file("priv/getpocket.term", io_lib:format("~p.", [Data]),
                    [write]).

-spec read_pocket_credentials() -> [proplists:property()] | no_return().
read_pocket_credentials() ->
    case file:consult("priv/getpocket.term") of
        {ok, [Keys]} ->
            Keys;
        Other ->
            verbose(error, "Unable to read getpocket "
                    "credentials - ~p!", [Other]),
            throw("Unable to read stored credentials!")
    end.

%%=============================================================================
%% Internal functionality
%%=============================================================================
build_downloader_command(Url) ->
    case get_downloader() of
        {ok, Path} ->
            lists:flatten(
              io_lib:format(
                "~s ~s ~s ~s",
                [Path, application:get_env(vffov, downloader_params, ""),
                 "-t ",
                 Url])
             );
        error ->
            verbose(error, "Unable to find downloader. Check configuration!", []),
            throw("Unable to find downloader!")
   end.

filter_filename_by_id([Id]) ->
    filter_filename_by_id(Id);
filter_filename_by_id(Id) ->
    PredFun = fun(F) -> string:str(F, Id) > 0 end,
    case lists:filter(PredFun, filelib:wildcard("*")) of
        []         -> [];
        [File | _] -> File
    end.

maybe_remove_from_playlist_file(Url) ->
    Path = vffov_utils:priv_dir(vffov) ++ "playlist.txt",
    case filelib:is_regular(Path) of
        true ->
            Lines    = readlines(Path),
            Modified = lists:filter(
                         fun (Line) ->
                                 string:str(Line, Url) =:= 0 end, Lines),
            ExistingLen = length(Lines),
            case ExistingLen =/= length(Modified) of
                true  ->
                    ok = file:delete(Path),
                    writelines(Path, Modified);
                false ->
                    ok
            end;
        false ->
            ok
    end.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Acc) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Acc;
        Line -> get_all_lines(Device, Acc ++ [Line])
    end.

writelines(FileName, []) ->
    file:write_file(FileName, <<>>, [write]);
writelines(FileName, Lines) ->
    file:write_file(FileName, lists:concat(Lines), [write]).
