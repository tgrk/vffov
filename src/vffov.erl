%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% VFFOV API
%%% TODO: * allow download url(url detection only)
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov).

%% API
-export([download/0,
         download/1,
         start/0,
         stop/0
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================
download() ->
    download("priv/playlist.txt").

download(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case filename:extension(Path) of
                ".json" -> parse(json, Bin);
                ".txt"  -> parse(txt, Bin);
                Other   -> throw({unknown_file_extension, Other})
            end;
        {error, Reason} ->
            lager:error("Unable to load download file ~s. Error ~p",
                        [Path, Reason])
    end.

start() ->
    [application:start(A) || A <- deps() ++ [vffov]].

stop() ->
    [application:stop(A) || A <- deps() ++ [vffov]].

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
parse(json, Bin) ->
    try
        case jiffy:decode(Bin) of
            {[{<<"list">>, List}]} ->
                handle_download(List);
            _ ->
                vffov_common:verbose(error, "Unable to parse JSON file!", [])
        end
    catch
        _:Reason ->
            vffov_common:verbose(error, "Unable to parse JSON file! Error ~p",
                                 [Reason])
    end;
parse(txt, Bin) ->
    try
        case string:tokens(erlang:binary_to_list(Bin), "\n") of
            []   ->
                empty_playlist;
            List ->
                handle_download(List)
        end
    catch
        _:Reason ->
            vffov_common:verbose(error, "Unable to parse text file! Error ~p",
                                 [Reason])
    end.

handle_download(List) ->
    case application:get_env(vffov, download_parallel, false) of
        true  -> lists:foreach(fun download_file/1, List);
        false -> queue_downloads(List)
    end.

download_file({[{<<"url">>, Url}]}) ->
    {_, _, Name} = erlang:now(),
    vffov_sup:start_worker(parallel, integer_to_list(Name), binary_to_list(Url));
download_file(Url) ->
    {_, _, Name} = erlang:now(),
    vffov_sup:start_worker(parallel, integer_to_list(Name), Url).

queue_downloads(List) ->
    {_, _, Name} = erlang:now(),
    vffov_sup:start_worker(queued, integer_to_list(Name), List).

deps() ->
    [compiler, syntax_tools, lager, jiffy, reloader].
