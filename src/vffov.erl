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
    case pocket_oauth() of
        {ok, {request_url, Url}} ->
            vffov_common:verbose(info, "Open following ~p in your browser and"
                                 " run again.~n", [Url]);
        {ok, [ConsumerKey, AccessToken]} ->
            case erlpocket:retrieve(ConsumerKey, AccessToken, Opts) of
                {ok, Result}    ->
                    {[{<<"status">>,1},
                      {<<"complete">>,1},
                      {<<"list">>, {Items}}, _]} = Result,
                    List = lists:map(
                             fun({_Id, {Item}}) ->
                                     binary_to_list(
                                       proplists:get_value(<<"given_url">>, Item))
                             end, Items),
                    handle_download(List);
                {error, Reason} ->
                    vffov_common:verbose(error,
                                         "Unable to get items from getpocket "
                                         "service! Error ~p", [Reason])
            end;
        error ->
            vffov_common:verbose(error,
                                 "Unable to reqeust authentification code! "
                                 "Check you consumer key!", [])
    end.

start() ->
    [application:start(A) || A <- deps()],

    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),

    application:ensure_all_started(erlpocket),
    application:set_env(erlpocket, verbose, true),

    application:start(vffov).

stop() ->
    [application:stop(A) || A <- deps() ++ [vffov]].

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
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

%%TODO: validate if consumer_key is not present
pocket_oauth() ->
    [Keys] = vffov_common:read_pocket_credentials(),
    ConsumerKey = proplists:get_value(consumer_key, Keys),
    case proplists:get_value(code, Keys) of
        [] ->
            case erlpocket:request_token(ConsumerKey, "http://www.wiso.cz/") of
                {ok, [{code, Code}]} ->
                    vffov_common:write_pocket_credentials(Code, ConsumerKey, []),
                    Url = erlpocket:get_authorize_url(Code, "http://www.wiso.cz/"),
                    {ok, {request_url, Url}};
                _ ->
                    error
            end;
        Code ->
            AccessToken = case proplists:get_value(access_token, Keys) =:= [] of
                              true ->
                                  Code = proplists:get_value(code, Keys),
                                  AuthResp = erlpocket:authorize(ConsumerKey, Code),
                                  {ok, [{access_token, AccessToken1},{username, _Username}]} = AuthResp,
                                  vffov_common:write_pocket_credentials(Code, ConsumerKey, AccessToken1),
                                  AccessToken1;
                              false ->
                                  proplists:get_value(access_token, Keys)
                          end,
            {ok, [ConsumerKey, AccessToken]}
    end.

deps() ->
    [compiler, syntax_tools, lager, jiffy, statman].
