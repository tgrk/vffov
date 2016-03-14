%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Common code shared between all workers
%%% @end
%%% Created : 29 Apr 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_utils).

%% API
-export([
         verbose/3,
         priv_dir/1,
         is_url/1,
         sanitize_urls/1,
         sanitize_url/1,
         move_to_download_dir/2,
         maybe_execute_command/2,
         get_downloader/0,
         open_downloader_port/1,
         close_downloader_port/1,
         read_pocket_credentials/0,
         write_pocket_credentials/3,
         process_sizes/0,
         total_memory/1
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
        [_] -> Url;
        [Base, Part2] ->
            case string:str(Base, "youtube.com") > 0 of
                true ->
                    %% for YT videos remove all except video id parameter
                    [{_K,V}] = lists:filtermap(
                                 fun(P) ->
                                         [K,V] = string:tokens(P, "="),
                                         case K =:= "v" of
                                             true  -> {true, {K,V}};
                                             false -> false
                                         end
                                 end,
                                 string:tokens(Part2, "&")
                                ),
                    Base ++ "?v=" ++ V;
                false ->
                    Url
            end
    end.

-spec move_to_download_dir(string(), pos_integer()) -> ok.
move_to_download_dir(Url, Start) ->
    %% find file by id in path (note that this is YT specific)
    [_ | Id] = string:tokens(Url, "="),
    [File] = lists:filter(
              fun(F) -> string:str(F, lists:concat(Id)) > 0 end,
              filelib:wildcard("*")
             ),
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

    %% finally move file
    TargetPath = filename:join(TargetDir, File),
    ok = file:rename(File, TargetPath),

    {ok,TargetPath}.

-spec maybe_execute_command(atom(), string()) -> ok.
maybe_execute_command(post, Path) ->
    try
        case application:get_env(vffov, post_download_cmd, undefined) of
            undefined -> ok;
            ""        -> ok;
            Command   -> os:cmd(hd(io_lib:format(Command, [Path])))
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
                      {start,  0},
                      {finish, 0}
                     ]),

    erlang:open_port(
      {spawn, build_downloader_command(Url)},
      [exit_status]
     ).

-spec close_downloader_port(atom() | port()) -> true.
close_downloader_port(Port) ->
    erlang:port_close(Port).

-spec get_downloader() -> string().
get_downloader() ->
    application:get_env(vffov, downloader_path, "/usr/bin/youtube-dl").

-spec write_pocket_credentials(string(), string(), string())
                              -> ok | {error,any()}.
write_pocket_credentials(Code, ConsumerKey, AccessToken) ->
    Data = [{code,         Code},
            {consumer_key, ConsumerKey},
            {access_token, AccessToken}],
    file:write_file("priv/getpocket.term", io_lib:format("~p.", [Data]),
                    [write]).

-spec read_pocket_credentials() -> list({atom(), any()}) | no_return().
read_pocket_credentials() ->
    case file:consult("priv/getpocket.term") of
        {ok, Keys} ->
            Keys;
        Other ->
            verbose(error, "Unable to read getpocket "
                                "credentials - ~p!", [Other]),
            throw("Unable to read stored credentials!")
    end.

-spec process_sizes() -> list({memory, integer()}).
process_sizes() ->
    Pids = lists:filtermap(
             fun ({Id, Pid, _, _}) when Id =:= vffov_queued_worker ->
                     {true, Pid};
                 ({_, Pid, _, Modules}) ->
                     case lists:member(vffov_parallel_worker, Modules) of
                         true -> {true, Pid};
                         false -> false
                     end;
                 (_) ->
                     false
             end,
             supervisor:which_children(vffov_sup)),
    lists:filtermap(fun (P) ->
                            case total_memory(P) of
                                undefined -> false;
                                Bytes     -> {true, {memory, Bytes}}
                            end
                    end, Pids).

-spec total_memory(pid()) -> integer() | undefined.
total_memory(Pid) ->
    case {process_info(Pid, memory), process_info(Pid, binary)} of
        {A, B} when A =:= undefined orelse B =:= undefined ->
            undefined;
        {{memory, Memory}, {binary, B}} ->
            Memory + lists:sum([Size || {_, Size, _} <- B])
    end.

%%=============================================================================
%% Internal functionality
%%=============================================================================
build_downloader_command(Url) ->
    lists:flatten(
      io_lib:format(
        "~s ~s ~s ~s",
        [get_downloader(),
         application:get_env(vffov, downloader_params, ""),
         "-t ",
         Url])
     ).
