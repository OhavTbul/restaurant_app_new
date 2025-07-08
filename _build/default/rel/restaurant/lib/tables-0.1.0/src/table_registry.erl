%% ------------------------------------------------------------------
%% table_registry.erl
%% Allocator: manages queue of waiting customers
%% ------------------------------------------------------------------

-module(table_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([request_table/1, notify_table_cleaned/1,cancel_request/1]).

start_link() -> %create new procces
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{queue => queue:new(), free_tables => []}}.


request_table(CustomerId) -> %customer ask to table
    gen_server:cast(?MODULE, {request_table, CustomerId}).

notify_table_cleaned(TableId) -> %got msg that the table is clean
    gen_server:cast(?MODULE, {table_cleaned, TableId}).

cancel_request(CustomerId) ->
    gen_server:cast(?MODULE, {cancel_request, CustomerId}).

handle_cast({request_table, CustomerId}, State) ->
    FreeTables = maps:get(free_tables, State),
    Queue = maps:get(queue, State),
    case FreeTables of
        [TableId | Rest] -> %if free table - seat customer
            table_fsm:seat_customer(TableId, CustomerId),
            {noreply, State#{free_tables => Rest}};
        [] ->
            NewQ = queue:in(CustomerId, Queue), %add to customer wueue if there is no free table
            {noreply, State#{queue => NewQ}}
    end;



handle_cast({table_cleaned, TableId}, State) ->
    Queue = maps:get(queue, State),
    FreeTables = maps:get(free_tables, State),
    case queue:out(Queue) of
        {{value, CustomerId}, NewQ} ->
            io:format("[table_registry] Assigning table ~p to waiting customer ~p~n", [TableId, CustomerId]),
            table_fsm:seat_customer(TableId, CustomerId),
            {noreply, State#{queue => NewQ}};
        {empty, _} ->
            io:format("[table_registry] Table ~p added to free list (no customers waiting)~n", [TableId]),
            {noreply, State#{free_tables => [TableId | FreeTables]}}
    end;

handle_cast({cancel_request, CustomerId}, State) ->
    Queue = maps:get(queue, State),
    NewQ = queue:filter(fun(ID) -> ID =/= CustomerId end, Queue),
    io:format("[table_registry] Customer ~p cancelled their request and was removed from the queue.~n", [CustomerId]),
    {noreply, State#{queue => NewQ}}.

handle_info(_, State) -> {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.


terminate(_, _) -> ok. %otp function
code_change(_, State, _) -> {ok, State}. %otp function