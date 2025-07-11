-module(customer_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/1, assign_table/2, place_order/2, done_eating/1, pay/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4,receive_order/2,handle_info/2]).

%% States
-export([idle/3, seated/3, waiting_food/3, eating/3, paying/3, leaving/3]).

%% Macros
-define(TABLE_TIMEOUT, 25000). % 25sec (Increased for debugging)
-define(ORDER_TIMEOUT, 20000). % 20sec
-define(EAT_TIMEOUT, 4000).  % 4sec
-define(HEARTBEAT_INTERVAL, 3000).  % 3sec

%%%===================
%%% API
%%%===================

start_link(ClientId) -> % Create and start a new customer FSM with given ID
    gen_statem:start_link({global, {customer_fsm, ClientId}}, ?MODULE, ClientId, []).

assign_table(ClientId, TableId) -> %sending msg to customer fsm - got table
    Name = {?MODULE, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global, Name}, {assign_table, TableId}).

place_order(ClientId, MenuItem) -> %sending msg to customer fsm - ordered
    Name = {?MODULE, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global,Name}, {order, MenuItem}).

done_eating(ClientId) -> %sending msg to customer fsm - done eating
    Name = {?MODULE, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global,Name}, done_eating).

pay(ClientId) -> %sending msg to customer fsm - done paing
    Name = {?MODULE, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global,Name}, paid).

receive_order(ClientId, _Order) ->
    Name = {?MODULE, ClientId},
    gen_statem:cast({global, Name}, food_arrived).


%%%===================
%%% gen_statem callbacks
%%%===================================================================

init(ClientId) ->
    case ets:lookup(customer_state, ClientId) of
        % Try to restore from ETS
        [{ClientId, SavedState}] ->
            io:format("[customer_fsm] Restoring customer ~p from ETS~n", [ClientId]),
            UpdatedState = SavedState#{client_id => ClientId, pid => self(), state_name => maps:get(state_name, SavedState, idle)},
            StateName = maps:get(state_name, SavedState, idle),
            erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),

            case StateName of
                idle ->
                    {ok, idle, UpdatedState, {state_timeout, ?TABLE_TIMEOUT, timeout_table}};
                seated ->
                    {ok, seated, UpdatedState, {state_timeout, ?ORDER_TIMEOUT, timeout_order}};
                eating ->
                    {ok, eating, UpdatedState, {state_timeout, ?EAT_TIMEOUT, done_eating}};
                _ -> % waiting_food, paying, leaving no state_timeout
                    {ok, StateName, UpdatedState}
            end;

        %% התחלה חדשה
        [] ->
            io:format("Customer ~p entered and waiting for table.~n", [ClientId]),
            gen_server:cast({global, table_registry}, {request_table, ClientId}),
            State = #{client_id => ClientId, pid => self(),pos => {0, 0}, state_name => idle},
            send_heartbeat(State),

            io:format("[DEBUG] Sending request_table to table_registry for customer ~p~n", [ClientId]),
            erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
            {ok, idle, State, {state_timeout, ?TABLE_TIMEOUT, timeout_table}}
    end.


callback_mode() -> state_functions. %fsm mood

terminate(_Reason, _State, _Data) -> ok. %OTP function
code_change(_OldVsn, StateName, Data, _Extra) -> {ok, StateName, Data}. %OTP function

%%%===================
%%% Heartbeat
%%%===================


send_heartbeat(State) -> 
    customer_sup:update_customer_state(maps:get(client_id, State), State),
    ok.


%%%===================
%%% States
%%%===================


%%%--- IDLE
idle(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

idle(cast, {assign_table, TableId}, State) -> % customer got table
    ClientId = maps:get(client_id, State),
    io:format("Customer ~p assigned to table ~p~n", [ClientId, TableId]),
    %% table POS known by table id
    TablePos = {0,0},  %% TODO: replace with real position later
    NewState = State#{table => TableId, pos => TablePos, state_name => seated},
    send_heartbeat(NewState),
    Task = #{type => take_order, table_id => TableId, client_id => ClientId},
    gen_server:cast({global, task_registry}, {add_task, Task}),
    io:format("Customer ~p sent take_order task.~n", [ClientId]),
    {next_state, seated, NewState, {state_timeout, ?ORDER_TIMEOUT, timeout_order}};

idle(state_timeout, timeout_table, State) -> %table timeout
    CustomerId = maps:get(client_id, State),
    io:format("Customer ~p gave up waiting for table (TIMEOUT). Sending cancel request.~n", [CustomerId]),
    gen_server:cast({global, table_registry}, {cancel_request, CustomerId}),
    NewState = State#{state_name => leaving},
    send_heartbeat(NewState),
    {next_state, leaving, NewState};

%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
idle(_Type, _Event, State) ->
    {keep_state, State}.

seated(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

seated(cast, {take_order, WaiterId}, State) ->
    ClientId = maps:get(client_id, State),
    TableId = maps:get(table, State),
    MenuItem = pizza,
    NewState1 = State#{waiter => WaiterId, order => MenuItem,state_name => waiting_food},
    Order = #{meal => MenuItem, table_id => TableId, client_id => ClientId},
    gen_statem:cast({global, {waiter_fsm, WaiterId}}, {client_order, Order}),
    io:format("Customer ~p (waiter ~p) ordered ~p and sent to waiter ~p~n", [ClientId, WaiterId, MenuItem, WaiterId]),
    send_heartbeat(NewState1),
    {next_state, waiting_food, NewState1};


seated(state_timeout, timeout_order, State) -> %order timeout
    CustomerId = maps:get(client_id, State),
    TableId = maps:get(table, State),
    io:format("Customer ~p waited too long to order. Leaving.~n", [CustomerId]),
    case TableId of
        undefined ->
            io:format("Customer ~p leaving without assigned table.~n", [CustomerId]);
        _ ->
            gen_statem:cast({global, {table_fsm, TableId}}, {free_table, CustomerId}),
            io:format("Customer ~p sent free_table message to table ~p.~n", [CustomerId, TableId])
    end,
    gen_statem:cast({global, task_registry}, {cancel_task, CustomerId}),
    io:format("Customer ~p canceled task.~n", [CustomerId]),
    NewState = State#{state_name => leaving},
    send_heartbeat(NewState),
    {next_state, leaving, NewState};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
seated(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- WAITING_FOOD
waiting_food(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

waiting_food(cast, food_arrived, State) -> %customer got food
    io:format("Customer ~p received food. Eating now~n", [maps:get(client_id, State)]),
    send_heartbeat(State),
    NewState = State#{state_name => eating},
    {next_state, eating, NewState, {state_timeout, ?EAT_TIMEOUT, done_eating}};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
waiting_food(_Type, _Event, State) ->
    {keep_state, State}.



%%%--- EATING
eating(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

eating(state_timeout, done_eating, State) ->
    CustomerId = maps:get(client_id, State),
    io:format("Customer ~p finished eating (timeout)~n", [CustomerId]),
    % שלח לעצמך הודעת 'paid' כדי להגיע למצב paying ולשחרר שולחן משם
    gen_statem:cast(self(), paid), % <--- שינוי קריטי!
    io:format("Customer ~p sent 'paid' message to self after eating.~n", [CustomerId]), % <--- הודעת דיבוג
    NewState = State#{state_name => paying},
    send_heartbeat(NewState),
    {next_state, paying, NewState};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
eating(_Type, _Event, State) ->
    {keep_state, State}.


%%%--- PAYING

paying(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

paying(cast, paid, State) ->
    ClientId = maps:get(client_id, State),
    TableId = maps:get(table, State),
    io:format("Customer ~p paid. Leaving.~n", [ClientId]),
    % שלח הודעה רק אם הלקוח אכן הושיב בשולחן
    case TableId of
        undefined ->
            io:format("Customer ~p paid and leaving without assigned table.~n", [ClientId]);
        _ ->
            gen_statem:cast({global, {table_fsm, TableId}}, {free_table, ClientId}),
            io:format("Customer ~p sent free_table message to table ~p.~n", [ClientId, TableId])
    end,
    %known pos to exit
    ExitPos = {999, 999}, %todo chane exit?
    NewState = State#{pos => ExitPos, state_name => leaving},
    send_heartbeat(NewState),
    {next_state, leaving, NewState};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
paying(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- LEAVING

%%%--- LEAVING
leaving(_Type, _Event, State) ->
    ClientId = maps:get(client_id, State),
    ets:delete(customer_state, ClientId),
    io:format("[customer_fsm] Customer ~p removed from ETS.~n", [ClientId]), 
    {stop, normal, State}.


handle_info(heartbeat_tick, State) ->
    send_heartbeat(State), % שליחה נכונה
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {noreply, State};

handle_info(Msg, State) -> % טיפול בהודעות info אחרות אם יהיו
    io:format("[customer_fsm] Customer ~p received unexpected info message: ~p in state ~p~n",
              [maps:get(client_id, State), Msg, maps:get(state_name, State, unknown)]),
    {noreply, State}.
