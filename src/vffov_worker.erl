%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Worker module that downloads videos based on specified Url
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/2,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {url}).

%%%============================================================================
%%% API
%%%============================================================================
start_link(Name, Url) ->
    gen_server:start_link({local, Name}, ?MODULE, [Url], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Url]) ->
    process_flag(trap_exit, true),
    {ok, #state{url = remove_https(Url)}, 0}.

handle_call(Call, From, State) ->
    vffov:verbose(error, "Unmatched call ~p from ~p", [Call, From]),
    {reply, invalid_call, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    vffov:verbose(error, "Unmatched cast ~p", [Cast]),
    {noreply, State}.
handle_info(timeout, #state{url = Url} = State) ->
    vffov:verbose(info, "Downloading video from url=~s", [Url]),
    open_port(build_command(Url)),
    {noreply, State};
handle_info({_Port, {data, []}}, State) ->
    {noreply, State};
handle_info({_Port, {data, "\n"}}, State) ->
    {noreply, State};
handle_info({_Port, {data, Data}}, #state{url = Url} = State) ->
    case string:substr(Data, 1, 11) == "\r[download]" of
        true  ->
           vffov:verbose(info, "Downloading ~s - ~s",
                         [Url, string:sub_string(Data, 15)]);
        false ->
          vffov:verbose(info, "~s - ~s", [Url, Data])
    end,
    {noreply, State};
handle_info({_Port, {exit_status,1}}, #state{url = Url} = State) ->
    vffov:verbose(info, "Downloading stopped ~s", [Url]),
    {stop, normal, State};
handle_info({_Port, {exit_status, 0}}, #state{url = Url} = State) ->
    vffov:verbose(info, "Finished downloading ~s", [Url]),
    {stop, normal, State};
handle_info({'EXIT', _Port, normal}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    vffov:verbose(error, "Unmatched info ~p, ~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
open_port(Cmd) ->
    erlang:open_port({spawn, Cmd}, [exit_status]).

build_command(Url) ->
    DownloaderPath = application:get_env(vffov, downloader_path, "youtube-dl"),
    DownloaderTemplate = case application:get_env(vffov, download_dir, "./") of
                             "./" -> [];
                             Path ->
                                 ["-o '" ++ Path ++ "%(title)s-%(id)s.%(ext)s'"]
                         end,
    DownloaderParams = application:get_env(vffov, downloader_params, ""),
    lists:flatten(
      io_lib:format("~s ~s ~s ~s",
                    [DownloaderPath, DownloaderTemplate, DownloaderParams, Url])
     ).

remove_https(Url) ->
    case string:str(Url, "https://") of
        0 -> Url;
        _ -> re:replace(Url, "https", "http", [{return, list}])
    end.
