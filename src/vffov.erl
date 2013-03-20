%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% VFFOV API
%%%
%%% TODO: * disable/enable parallel download?
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov).

%% API
-export([download/1,
         verbose/3,
         start/0,
         stop/0
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================
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

verbose(Type, Msg, Args) ->
    case application:get_env(vffov, enable_logging, false) of
        false -> io:format(Msg ++ "\n", Args);
        true  -> lager:log(Type, Msg, Args)
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
                lists:foreach(fun download_file/1, List),
                ok;
            _ ->
                verbose(error, "Unable to parse JSON file!", [])
        end
    catch
        _:Reason ->
            verbose(error, "Unable to parse JSON file! Error ~p", [Reason])
    end;
parse(txt, Bin) ->
    try
        List = string:tokens(erlang:binary_to_list(Bin), "\n"),
        lists:foreach(fun download_file/1, List),
        ok
                catch
        _:Reason ->
            verbose(error, "Unable to parse text file! Error ~p", [Reason])
    end.

download_file({[{<<"url">>, Url}]}) ->
    {_, _, Name} = erlang:now(),
    vffov_sup:start_worker(integer_to_list(Name), binary_to_list(Url));
download_file(Url) ->
    {_, _, Name} = erlang:now(),
    vffov_sup:start_worker(integer_to_list(Name), Url).

deps() ->
    [compiler, syntax_tools, lager, jiffy, reloader].
