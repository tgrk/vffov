%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Manages subscribers for EventSource based notifications
%%% @end
%%% Created : 12 Aug 2014 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(vffov_notify_server).
-behaviour(gen_server).

%% API
-export([start_link/0, add_client/1, add_notification/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {clients :: list(),
                queue   :: queue:queue(binary())}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok,pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%TODO: ref type?
-spec add_client(_) -> any().
add_client(Ref) ->
    gen_server:call(?MODULE, {add_client, Ref}).

-spec add_notification(binary()) -> ok.
add_notification(Message) ->
    gen_server:cast(?MODULE, {add_notification, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    timer:send_interval(1000, pull),
    {ok, #state{clients = [], queue = queue:new()}}.

handle_call({add_client, Ref}, _From, #state{clients = Clients} = State) ->
    {reply, ok, State#state{clients = [Ref | Clients]}}.

handle_cast({add_notification, Message}, #state{queue = Queue} = State) ->
    {noreply, State#state{queue = queue:in(Message, Queue)}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(pull, #state{clients = []} = State) ->
    {noreply, State};
handle_info(pull, #state{clients = Clients, queue = Queue} = State) ->
    {NewQueue, NewClients} = process_queue(Clients, Queue),
    {noreply, State#state{clients = NewClients, queue = NewQueue}};
handle_info(_, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_queue(Clients, Queue) ->
    case queue:is_empty(Queue) of
        true  ->
            {Queue, Clients};
        false ->
            {{value, Message}, NewQueue} = queue:out(Queue),
            Chunk = ["data: ", binary_to_list(Message), "\n\n"],
            NewClients = notify_subscribers(Clients, Chunk),
            process_queue(NewClients, NewQueue)
    end.

notify_subscribers(Clients, Chunk) ->
    lists:filtermap(
      fun (Client) ->
              case elli_request:send_chunk(Client, Chunk) of
                  ok ->
                      {true, Client};
                  {error, closed} ->
                      elli_request:send_chunk(Client, <<"">>),
                      false;
                  {error, timeout} ->
                      false
              end
      end, Clients).
