-module(customer_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/1, assign_table/2, place_order/2, done_eating/1, pay/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4,receive_order/2,handle_info/2]).

%% States
-export([idle/3, seated/3, waiting_food/3, eating/3, paying/3, leaving/3]).

%% Macros
-define(TABLE_TIMEOUT, 5000). % 5sec
-define(ORDER_TIMEOUT, 8000). % 8sec
-define(EAT_TIMEOUT, 4000).  % 4sec
-define(HEARTBEAT_INTERVAL, 3000).  % 3sec

%%%===================
%%% API
%%%===================

start_link(ClientId) -> % Create and start a new customer FSM with given ID
    gen_statem:start_link({global, {customer_fsm, ClientId}}, ?MODULE, ClientId, []).

assign_table(ClientId, TableId) -> %sending msg to customer fsm - got table
    Name = {customer_fsm, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global, Name}, {assign_table, TableId}).

place_order(ClientId, MenuItem) -> %sending msg to customer fsm - ordered
    Name = {customer_fsm, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global,Name}, {order, MenuItem}).

done_eating(ClientId) -> %sending msg to customer fsm - done eating
    Name = {customer_fsm, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global,Name}, done_eating).

pay(ClientId) -> %sending msg to customer fsm - done paing
    Name = {customer_fsm, ClientId}, % בניית שם התהליך מה-ID
    gen_statem:cast({global,Name}, paid).

receive_order(ClientId, _Order) ->
    Name = {customer_fsm, ClientId},
    gen_statem:cast({global, Name}, food_arrived).


%%%===================
%%% gen_statem callbacks
%%%===================

init(ClientId) ->
    case ets:lookup(customer_state, ClientId) of
        %% שחזור מצב
        [{ClientId, SavedState}] ->
            io:format("Customer ~p restarted with previous state.~n", [ClientId]),
            StateName = maps:get(state, SavedState, idle),
            erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),

            %% הפעלה מחדש של הטיימר המתאים למצב
            case StateName of
                idle ->
                    {ok, idle, SavedState, {state_timeout, ?TABLE_TIMEOUT, timeout_table}};
                seated ->
                    {ok, seated, SavedState, {state_timeout, ?ORDER_TIMEOUT, timeout_order}};
                eating ->
                    {ok, eating, SavedState, {state_timeout, ?EAT_TIMEOUT, done_eating}};
                _ -> % למצבים waiting_food, paying, leaving אין state_timeout
                    {ok, StateName, SavedState}
            end;

        %% התחלה חדשה
        [] ->
            io:format("Customer ~p entered and waiting for table.~n", [ClientId]),
            table_registry:request_table(ClientId),
            State = #{client_id => ClientId, pos => {0, 0}, state => idle}, % חשוב להוסיף את המצב
            erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
            {ok, idle, State, {state_timeout, ?TABLE_TIMEOUT, timeout_table}}
    end.


callback_mode() -> state_functions. %fsm mood

terminate(_Reason, _State, _Data) -> ok. %OTP function
code_change(_OldVsn, StateName, Data, _Extra) -> {ok, StateName, Data}. %OTP function

%%%===================
%%% Heartbeat
%%%===================

send_heartbeat(State) -> %update sup
    customer_sup:update_customer_state(maps:get(client_id, State), State),
    ok.


%%%===================
%%% States
%%%===================


%%%--- IDLE

idle(cast, {assign_table, TableId}, State) -> %customer got table
    io:format("Customer ~p assigned to table ~p~n", [maps:get(client_id, State), TableId]),
    %% table POS knwom by table id
    %TablePos = table_position(TableId),  %% todo need to add function for table pos
    TablePos ={0,0},
    NewState = State#{table => TableId, pos => TablePos},
    send_heartbeat(NewState),
    {next_state, seated, NewState, {state_timeout, ?ORDER_TIMEOUT, timeout_order}};


idle(state_timeout, timeout_table, State) -> %table timeout
    CustomerId = maps:get(client_id, State),
    io:format("Customer ~p gave up waiting for table.~n", [CustomerId]),
    table_registry:cancel_request(CustomerId),
    {next_state, leaving, State};

%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
idle(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- SEATED

%seated(cast, {order, MenuItem}, State) -> %waiter come to take order
%    io:format("Customer ~p ordered ~p~n", [maps:get(client_id, State), MenuItem]),
%    NewState = State#{order => MenuItem},
%    send_heartbeat(NewState),
%    {next_state, waiting_food, NewState};


%simulation:

seated(cast, {order, MenuItem}, State) ->
    TableId = maps:get(table, State),
    Order = #{meal => MenuItem, table_id => TableId, client_id => maps:get(client_id, State)},
    MachineId = machine_fsm_m1,
    machine_fsm:machine_order(MachineId, Order),
    io:format("Customer ~p ordered ~p, sent to machine ~p~n", [maps:get(client_id, State), Order, MachineId]),
    NewState = State#{order => MenuItem},
    send_heartbeat(NewState),
    {next_state, waiting_food, NewState};


seated(state_timeout, timeout_order, State) -> %order timeout
    io:format("Customer ~p waited too long to order. Leaving.~n", [maps:get(client_id, State)]),
    {next_state, leaving, State};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
seated(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- WAITING_FOOD

waiting_food(cast, food_arrived, State) -> %customer got food
    io:format("Customer ~p received food. Eating now~n", [maps:get(client_id, State)]),
    send_heartbeat(State),
    {next_state, eating, State, {state_timeout, ?EAT_TIMEOUT, done_eating}};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
waiting_food(_Type, _Event, State) ->
    {keep_state, State}.



%%%--- EATING

eating(state_timeout, done_eating, State) ->
    io:format("Customer ~p finished eating (timeout)~n", [maps:get(client_id, State)]),
    {next_state, paying, State};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
eating(_Type, _Event, State) ->
    {keep_state, State}.


%%%--- PAYING

paying(cast, paid, State) ->
    io:format("Customer ~p paid. Leaving.~n", [maps:get(client_id, State)]),
    %known pos to exit
    ExitPos = {999, 999}, %todo chane exit?
    NewState = State#{pos => ExitPos},
    send_heartbeat(NewState),
    {next_state, leaving, NewState};


%%If an unexpected event arrives by mistake, simply ignore it and avoid crashing.
paying(_Type, _Event, State) ->
    {keep_state, State}.

%%%--- LEAVING

leaving(_Type, _Event, State) ->
    {stop, normal, State}.


handle_info(heartbeat_tick, State) ->
    send_heartbeat(State), % שליחת העדכון
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick), % קביעת הטיימר הבא
    {noreply, State};

handle_info(_, State) -> % טיפול בהודעות info אחרות אם יהיו
    {noreply, State}.
