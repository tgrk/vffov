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
    case filelib:is_regular(vffov_common:get_downloader()) of
        true  ->
            case filelib:is_regular(L) of
                true  -> download_1(L);
                false -> process_url_list(L)
            end;
        false ->
            lager:error("Clive downloader not found!")
    end.

start() ->
    [application:start(A) || A <- deps() ++ [vffov]].

stop() ->
    [application:stop(A) || A <- deps() ++ [vffov]].

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
download_1(Input) ->
    case is_url(Input) of
        true  ->
            %% handle input form console
            handle_download([Input]);
        false ->
            %% handle file input
            handle_download(parse(Input))
    end.

process_url_list(L) when is_list(L) ->
    case length(L) > 0 andalso is_url(hd(L)) of
        true  -> handle_download(L);
        false -> handle_download([L])
    end.

handle_download(List) ->
    case application:get_env(vffov, download_parallel, false) of
        true  -> lists:foreach(fun download_file/1, List);
        false -> queue_downloads(List)
    end.

download_file({[{<<"url">>, Url}]}) ->
    vffov_sup:start_worker(vffov_parallel_worker, Url);
download_file(Url) ->
    vffov_sup:start_worker(vffov_parallel_worker, Url).

queue_downloads(List) ->
    vffov_sup:start_worker(vffov_queued_worker, List).

is_url(S) when is_list(S) ->
    string:str(S, "http://") > 0 orelse string:str("https://", S) > 0;
is_url(_S) ->
    false.

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
            vffov_common:verbose(
              error, "Unable to load playlist file! Error ~p",
              [Reason]
             )
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
    [compiler, syntax_tools, lager, jiffy].
