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
load() ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(erlpocket),
    application:set_env(erlpocket, verbose, true).

auth() ->
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
            case proplists:get_value(access_token, Keys) =:= [] of
                true ->
                    Code = proplists:get_value(code, Keys),
                    AuthResp = erlpocket:authorize(ConsumerKey, Code),
                    {ok, [{access_token, AccessToken1},{username, _Username}]} = AuthResp,
                    vffov_common:write_pocket_credentials(Code, ConsumerKey, AccessToken1);
                false ->
                    proplists:get_value(access_token, Keys)
            end,
            ok
    end.

list(Options) ->
    {ConsumerKey, AccessToken} = get_credentials(),
    case erlpocket:retrieve(ConsumerKey, AccessToken, Options) of
        {ok, _, Result}    ->
            {[{<<"status">>,1},
              {<<"complete">>,1},
              {<<"list">>, {Items}}, _]} = Result,
            case Items of
                [] -> empty;
                Items ->
                    lists:map(
                      fun({Id, {Item}}) ->
                              {binary_to_list(Id),
                               binary_to_list(
                                 proplists:get_value(<<"given_url">>, Item))
                              }
                      end, Items)
            end;
        {error, Reason} ->
            {error, {unable_to_list, Reason}}
    end.

mark_done(ItemId) ->
    {ConsumerKey, AccessToken} = get_credentials(),
    io:format("debug: mark_done=~p~n", [ItemId]),
    case erlpocket:delete(ConsumerKey, AccessToken, ItemId) of
        {ok,{[{<<"action_results">>,[true]},{<<"status">>,1}]}} -> true;
        _ -> false
    end.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
get_credentials() ->
    [Keys] = vffov_common:read_pocket_credentials(),
    ConsumerKey = proplists:get_value(consumer_key, Keys),
    AccessToken = proplists:get_value(access_token, Keys),
    {ConsumerKey, AccessToken}.
