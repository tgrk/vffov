%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Main VFFOV application
%%% @end
%%% Created : 5 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([print_welcome/0]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
-spec start(_,_) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_StartType, _StartArgs) ->
    case vffov_sup:start_link() of
        {ok, _Pid} = Result ->
            print_welcome(),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(any()) -> 'ok'.
stop(_State) ->
    ok.

-spec print_welcome() -> ok.
print_welcome() ->
    Print = fun (Line, Args) -> vffov_utils:verbose(info, Line, Args) end,
    Print("+-------------------------------------------------+", []),
    Print("| Welcome to VFFOV                                |", []),
    Print("+-------------------------------------------------+", []),
    Print("| * REST API - http://127.0.0.1:8081/             |", []),
    Print("| * Statman  - http://127.0.0.1:8081/statman      |", []),
    Print("+-------------------------------------------------+", []),
    Print("| Commands:                                       |", []),
    Print("+-------------------------------------------------+", []),
    HelpFun = fun ({module_info, _Arity}) ->
                      ignore;
                  ({Name, Arity}) ->
                      Print("  * ~s/~s" , [Name, integer_to_list(Arity)])
              end,
    lists:foreach(HelpFun, vffov:module_info(exports)),
    Print("+-------------------------------------------------+", []),
    ok.
