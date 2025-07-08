-module(table_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([request_table/1, notify_table_cleaned/1,cancel_request/1, get_state/0]). % get_state/0 exported

start_link() -> %create new procces
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{queue => queue:new(), free_tables => []}}.


request_table(CustomerId) -> %customer ask to table
    gen_server:cast({global, ?MODULE}, {request_table, CustomerId}). % Changed to global call

notify_table_cleaned(TableId) -> %got msg that the table is clean
    gen_server:call({global, ?MODULE}, {table_cleaned_sync, TableId}). % Changed to global call (for sync debug)

cancel_request(CustomerId) ->
    gen_server:cast({global, ?MODULE}, {cancel_request, CustomerId}). % Changed to global call


% --- Public API for debugging state ---
get_state() ->
    gen_server:call({global, ?MODULE}, get_state_sync).


% --- gen_server callbacks ---

handle_cast({request_table, CustomerId}, State) ->
    FreeTables = maps:get(free_tables, State),
    Queue = maps:get(queue, State),
    io:format("[table_registry] Received request for table from customer ~p. Free tables: ~p, Queue length: ~p~n", [CustomerId, FreeTables, queue:len(Queue)]),
    case FreeTables of
        [TableId | Rest] -> %if free table - seat customer
            io:format("[table_registry] Free table ~p found, assigning to customer ~p.~n", [TableId, CustomerId]),
            table_fsm:seat_customer(TableId, CustomerId),
            {noreply, State#{free_tables => Rest}};
        [] ->
            NewQ = queue:in(CustomerId, Queue), %add to customer wueue if there is no free table
            io:format("[table_registry] No free tables, customer ~p added to queue. New queue length: ~p~n", [CustomerId, queue:len(NewQ)]),
            {noreply, State#{queue => NewQ}}
    end;



handle_cast({table_cleaned, TableId}, State) ->
    
    Queue = maps:get(queue, State),
    FreeTables = maps:get(free_tables, State),
    io:format("[table_registry] Received table_cleaned for table ~p. Current Queue length: ~p, Free tables: ~p~n", [TableId, queue:len(Queue), FreeTables]),
    case queue:out(Queue) of
        {{value, CustomerId}, NewQ} ->
            io:format("[table_registry] Assigning table ~p to waiting customer ~p. New queue length: ~p~n", [TableId, CustomerId, queue:len(NewQ)]),
            table_fsm:seat_customer(TableId, CustomerId),
            {noreply, State#{queue => NewQ}};
        {empty, _} ->
            NewFreeTables = [TableId | FreeTables],
            io:format("[table_registry] Queue is empty. Table ~p added to free list. New free tables: ~p~n", [TableId, NewFreeTables]),
            {noreply, State#{free_tables => NewFreeTables}}
    end;

handle_cast({cancel_request, CustomerId}, State) ->
    Queue = maps:get(queue, State),
    NewQ = queue:filter(fun(ID) -> ID =/= CustomerId end, Queue),
    io:format("[table_registry] Customer ~p cancelled their request and was removed from the queue.~n", [CustomerId]),
    {noreply, State#{queue => NewQ}}.

handle_info(_, State) -> {noreply, State}.

% --- handle_call clauses (must be contiguous and ordered from most specific to least specific) ---

% Specific handle_call for synchronous table cleaned message
handle_call({table_cleaned_sync, TableId}, _From, State) ->
    Queue = maps:get(queue, State),
    FreeTables = maps:get(free_tables, State),
    io:format("[table_registry] (SYNC_CALL) Received table_cleaned_sync for table ~p. Current Queue length: ~p, Free tables: ~p~n", [TableId, queue:len(Queue), FreeTables]),
    case queue:out(Queue) of
        {{value, CustomerId}, NewQ} ->
            io:format("[table_registry] (SYNC_CALL) Assigning table ~p to waiting customer ~p. New queue length: ~p~n", [TableId, CustomerId, queue:len(NewQ)]),
            table_fsm:seat_customer(TableId, CustomerId),
            {reply, ok, State#{queue => NewQ}};
        {empty, _} ->
            NewFreeTables = [TableId | FreeTables],
            io:format("[table_registry] (SYNC_CALL) Queue is empty. Table ~p added to free list. New free tables: ~p~n", [TableId, NewFreeTables]),
            {reply, ok, State#{free_tables => NewFreeTables}}
    end;

% handle_call for get_state_sync
handle_call(get_state_sync, _From, State) ->
    {reply, State, State}; % <--- Added semicolon here

% Generic handle_call for any other requests (must be last)
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.


terminate(_, _) -> ok. %otp function
code_change(_, State, _) -> {ok, State}. %otp function
