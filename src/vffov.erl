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
         start/0,
         stop/0
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================
download() ->
    download(vffov_common:priv_dir(vffov) ++ "playlist.txt").

download(L) when is_list(L) ->
    process_url_list(L);
download(L) ->
    case is_url(L) of
        true  ->
            handle_download([L]);
        false ->
            case filelib:is_regular(vffov_common:get_downloader()) of
                true  -> download_1(L);
                false -> lager:error("Clive downloader not found!")
            end
     end.

start() ->
    [application:start(A) || A <- deps() ++ [vffov]].

stop() ->
    [application:stop(A) || A <- deps() ++ [vffov]].

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
download_1(Path) ->
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

process_url_list(L) when is_list(L) ->
    case length(L) > 0 andalso is_url(hd(L)) of
        true  -> handle_download(L);
        false -> handle_download([L])
    end.

is_url(S) when is_list(S) ->
    string:str(S, "http://") > 0 orelse string:str("https://", S) > 0;
is_url(_S) ->
    false.

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
            []   -> empty_playlist;
            List -> handle_download(List)
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

%TODO: unify this functions to have same input
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
    [compiler, syntax_tools, lager, jiffy].
