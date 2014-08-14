%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Plugin for getting data from GetPocket.com over API
%%% @end
%%% Created : 25 Feb 2014 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_getpocket).

%% API
-export([load/0, auth/0, list/1, mark_done/1]).

%%%============================================================================
%%% API
%%%============================================================================
-spec load() -> ok.
load() ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(erlpocket),
    application:set_env(erlpocket, verbose, false).

-spec auth() ->  ok | {ok, {request_url, string()}} | error.
auth() ->
    [Keys] = vffov_utils:read_pocket_credentials(),
    ConsumerKey = proplists:get_value(consumer_key, Keys),
    case proplists:get_value(code, Keys) of
        [] ->
            case erlpocket:request_token(ConsumerKey, "http://www.wiso.cz/") of
                {ok, [{code, Code}]} ->
                    vffov_utils:write_pocket_credentials(Code, ConsumerKey, []),
                    Url = erlpocket:get_authorize_url(Code, "http://www.wiso.cz/"),
                    {ok, {request_url, Url}};
                _ ->
                    error
            end;
        Code ->
            case proplists:get_value(access_token, Keys) =:= [] of
                true ->
                    Code = proplists:get_value(code, Keys),
                    AuthResp = erlpocket:authorize(ConsumerKey, Code),
                    {ok, [{access_token, AccessToken1},
                          {username, _Username}]} = AuthResp,
                    vffov_utils:write_pocket_credentials(Code, ConsumerKey,
                                                         AccessToken1);
                false ->
                    proplists:get_value(access_token, Keys)
            end,
            ok
    end.

-spec list([{atom(), any()}]) ->
                  [any()] | empty | {error, {unable_to_list, any()}}.
list(Options) ->
    {ConsumerKey, AccessToken} = get_credentials(),
    case erlpocket:retrieve(ConsumerKey, AccessToken, Options) of
        {ok, _, Result}    ->
            case Result of
                {[{<<"status">>,2},
                  {<<"complete">>,1},
                  {<<"list">>,[]},
                  {<<"since">>, _}]} ->
                    empty;
                {[{<<"status">>,1},
                  {<<"complete">>,1},
                  {<<"list">>, {Items}}, _]} ->
                    lists:filtermap(
                      fun({Id, {Item}}) ->
                              %% download only youtube based videos
                              Url = binary_to_list(
                                      proplists:get_value(<<"given_url">>, Item)),
                              case string:str(Url, "youtube.com") > 0 of
                                  true  -> {true, {binary_to_list(Id), Url}};
                                  false -> false
                              end
                      end, Items)
            end;
        {error, Reason} ->
            {error, {unable_to_list, Reason}}
    end.

-spec mark_done(string()) -> boolean().
mark_done(ItemId) ->
    {ConsumerKey, AccessToken} = get_credentials(),
    vffov_utils:verbose(info, "Marking video with id=~p as done.", [ItemId]),
    case erlpocket:delete(ConsumerKey, AccessToken, list_to_binary(ItemId)) of
        {ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}} -> true;
        _ -> false
    end.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
get_credentials() ->
    [Keys] = vffov_utils:read_pocket_credentials(),
    ConsumerKey = proplists:get_value(consumer_key, Keys),
    AccessToken = proplists:get_value(access_token, Keys),
    {ConsumerKey, AccessToken}.
