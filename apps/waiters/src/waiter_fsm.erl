%% ------------------------------------------------------------------
%% waiter_fsm.erl
%% Waiter FSM based on provided state machine diagram.
%% Handles taking orders, serving meals, and movement.
%% ------------------------------------------------------------------

-module(waiter_fsm).
-behaviour(gen_statem).

%% API
-export([start_link/1, take_order/3, serve_meal/4, upgrade/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4, handle_info/2]).

%% States
-export([idle/3, taking_order/3, send_order/3, pick_up_meal/3, serving/3]).


-define(ORDER_TIME_BASE, 10000).   % Base time for taking an order 10 sec
-define(WALK_TIME_BASE, 8000).    % Base time for walking 8 sec
-define(SERVE_TIME_BASE, 6000).   % Base time for serving a meal 6 sec

% Speed upgrade levels (higher level = faster times)
-define(SPEED_LEVEL_0, 0). % Level 0
-define(SPEED_LEVEL_1, 1). % Level 1
-define(SPEED_LEVEL_2, 2). % Level 2

% Define ETS table for waiter state
-define(TABLE, waiter_state).


%%%===================================================================
%%% Public API
%%%===================================================================

start_link(WaiterId) ->
    Name = {?MODULE, WaiterId},
    gen_statem:start_link({global, Name}, ?MODULE, WaiterId, []). 

% עדכון החתימה והגוף של הפונקציה
take_order(WaiterId, TableId, ClientId) ->
    Name = {?MODULE, WaiterId},
    TaskMap = #{
        type => take_order,
        table_id => TableId,
        client_id => ClientId
    },
    gen_statem:cast({global, Name}, {task, TaskMap}).

% עדכון החתימה והגוף של הפונקציה
serve_meal(WaiterId, TableId, ClientId, Meal) ->
    Name = {?MODULE, WaiterId},
    TaskMap = #{
        type => serve_meal,
        table_id => TableId,
        client_id => ClientId,
        meal => Meal
    },
    gen_statem:cast({global, Name}, {task, TaskMap}).

upgrade(WaiterId) ->
    Name = {?MODULE, WaiterId},
    gen_statem:cast({global, Name}, upgrade).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init(WaiterId) ->
    % Try to restore from ETS
    case ets:lookup(?TABLE, WaiterId) of
        [{_, SavedState}] ->
            io:format("[waiter_fsm] Restoring waiter ~p from ETS~n", [WaiterId]),
            State = SavedState#{waiter_id => WaiterId},
            CurrentTask = maps:get(current_task, SavedState, undefined),
            SpeedLevel = maps:get(speed_level, SavedState, 0),
            case CurrentTask of
                undefined ->
                    {ok, idle, State};
                {take_order, TableId} ->
                    WalkTime = get_adjusted_time(walk, SpeedLevel),
                    io:format("[waiter_fsm] Waiter ~p resuming task to take order from table ~p. Walking to table...~n", [WaiterId, TableId]),
                    {ok, walk_to_table_order, State, {state_timeout, WalkTime, walk_time}};
                {serve_meal, TableId, Meal} ->
                    WalkTime = get_adjusted_time(walk, SpeedLevel),
                    io:format("[waiter_fsm] Waiter ~p resuming task to serve meal ~p to table ~p. Walking to kitchen...~n", [WaiterId, Meal, TableId]),
                    {ok, walk_to_kitchen, State, {state_timeout, WalkTime, walk_time}}
            end;
        [] ->
            % First time startup
            io:format("[waiter_fsm] Waiter ~p starting (idle).~n", [WaiterId]),
            State = #{
                waiter_id => WaiterId,
                speed_level => 0, % Initial speed level
                current_task => undefined,
                current_client_id  => undefined,
                current_table_id => undefined,
                current_meal => undefined
            },
            gen_server:cast({global, task_registry}, {waiter_ready, WaiterId}),
            {ok, idle, State}
    end.

callback_mode() -> state_functions.

terminate(_Reason, _StateName, _Data) ->
    io:format("[waiter_fsm] Waiter terminating: ~p~n", [_Reason]),
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.



handle_info(heartbeat_tick, State) ->
    % Example: Waiter could report status periodically
    io:format("[waiter_fsm] Waiter ~p heartbeat in state ~p~n", [maps:get(waiter_id, State), gen_statem:which_state(self())]),
    %todo - send to sup for update ets
    erlang:send_after(5000, self(), heartbeat_tick), % Reschedule heartbeat
    {keep_state, State};

handle_info(Msg, State) -> % Catch-all for other info messages
    io:format("[waiter_fsm] Waiter ~p received unexpected info message: ~p in state ~p~n",
              [maps:get(waiter_id, State), Msg, gen_statem:which_state(self())]),
    {keep_state, State}.


%%%===================================================================
%%% Helper Functions
%%%===================================================================

% Returns the adjusted time in milliseconds based on action type and speed level
get_adjusted_time(ActionType, SpeedLevel) ->
    case {ActionType, SpeedLevel} of
        {order, ?SPEED_LEVEL_0} -> 10000; % 10 seconds
        {order, ?SPEED_LEVEL_1} -> 8000;  % 8 seconds
        {order, ?SPEED_LEVEL_2} -> 6000;  % 6 seconds

        {walk, ?SPEED_LEVEL_0} -> 8000;  % 8 seconds
        {walk, ?SPEED_LEVEL_1} -> 6000;  % 6 seconds
        {walk, ?SPEED_LEVEL_2} -> 4000;  % 4 seconds

        {serve, ?SPEED_LEVEL_0} -> 6000;  % 6 seconds
        {serve, ?SPEED_LEVEL_1} -> 4000;  % 4 seconds
        {serve, ?SPEED_LEVEL_2} -> 2000;  % 2 seconds

        % Default case for unexpected ActionType or SpeedLevel
        {_, _} -> 5000 % Default to 5 seconds if no specific match
    end.


%%%===================================================================
%%% State Definitions
%%%===================================================================

%%%--- IDLE
%%%--- IDLE
idle(cast, {task, TaskMap}, State) when is_map(TaskMap) ->
    WaiterId = maps:get(waiter_id, State),
    SpeedLevel = maps:get(speed_level, State),
    TableId = maps:get(table_id, TaskMap),
    ClientId = maps:get(client_id, TaskMap),
    WalkTime = get_adjusted_time(walk, SpeedLevel),

    case maps:get(type, TaskMap) of
        take_order ->
            io:format("[waiter_fsm] Waiter ~p assigned to take order from table ~p (walk + order).~n", [WaiterId, TableId]),
            NewState = State#{current_table_id := TableId, current_client_id := ClientId, current_task := {take_order, TableId}},
            {next_state, taking_order, NewState, {state_timeout, WalkTime, get_to_table_timeout}};

        serve_meal ->
            Meal = maps:get(meal, TaskMap), % Assuming the meal is passed in the task map
            io:format("[waiter_fsm] Waiter ~p received task to serve meal ~p to table ~p.~n", [WaiterId, Meal, TableId]),
            NewState = State#{current_table_id := TableId, current_client_id := ClientId, current_meal := Meal, current_task := {serve_meal, TableId, Meal}},
            {next_state, pick_up_meal, NewState, {state_timeout, WalkTime, walk_time}};
            
        _Other ->
            io:format("[waiter_fsm] Waiter ~p received unknown task type: ~p~n", [WaiterId, maps:get(type, TaskMap)]),
            {keep_state, State}
    end;

idle(cast, upgrade, State) ->
    WaiterId = maps:get(waiter_id, State),
    OldLevel = maps:get(speed_level, State),
    NewLevel = min(OldLevel + 1, 2), % Max speed level is 2
    io:format("[waiter_fsm] Waiter ~p upgraded speed from level ~p to ~p.~n", [WaiterId, OldLevel, NewLevel]),
    NewState = State#{speed_level := NewLevel},
    waiter_sup:update_waiter_state(WaiterId, NewState),
    {keep_state, NewState};

idle(_Type, _Event, State) ->
    io:format("[waiter_fsm] Waiter ~p received unhandled event ~p in idle state~n", [maps:get(waiter_id, State), _Event]),
    {keep_state, State}.


%%%--- TAKING_ORDER
taking_order(state_timeout, get_to_table_timeout, State) ->
    WaiterId = maps:get(waiter_id, State),
    TableId = maps:get(current_table_id, State),
    ClientId = maps:get(current_client_id, State),
    io:format("[waiter_fsm] Waiter ~p start taking order from table ~p.~n", [WaiterId, TableId]),
    gen_server:cast({global, {customer_fsm,ClientId}}, {take_order,WaiterId}),
    {next_state, send_order, State, {state_timeout, 20000, customer_reply_timeout}};

taking_order(cast, upgrade, State) ->
    WaiterId = maps:get(waiter_id, State),
    OldLevel = maps:get(speed_level, State),
    NewLevel = min(OldLevel + 1, 2),
    io:format("[waiter_fsm] Waiter ~p upgraded speed from level ~p to ~p while taking order.~n", [WaiterId, OldLevel, NewLevel]),
    NewState = State#{speed_level := NewLevel},
    waiter_sup:update_waiter_state(WaiterId, NewState),
    {keep_state, NewState};

taking_order(_Type, _Event, State) ->
    {keep_state, State}.

%%--send order
%% in waiter_fsm.erl, send_order/3
send_order(cast, {client_order, Order}, State) ->
    WaiterId = maps:get(waiter_id, State),
    io:format("[waiter_fsm] Waiter ~p send meal to order queue.~n", [WaiterId]),
    gen_server:cast({global, order_registry}, {add_order,Order}),
    NewState = State#{current_task => undefined, current_meal => undefined, current_table_id => undefined, current_client_id => undefined},
    gen_server:cast({global, task_registry}, {waiter_ready, WaiterId}),
    {next_state, idle, NewState};


%% טיפול במצב שהלקוח לא ענה בזמן
send_order(state_timeout, customer_reply_timeout, State) ->
    WaiterId = maps:get(waiter_id, State),
    TableId = maps:get(current_table_id, State),
    io:format("[waiter_fsm] Waiter ~p: Customer at table ~p did not respond (timeout). Returning to idle.~n", [WaiterId, TableId]),
    gen_server:cast({global, task_registry}, {waiter_ready, WaiterId}),
    NewState = State#{current_task := undefined, current_meal := undefined, current_table_id := undefined, current_client_id := undefined},
    {next_state, idle, NewState};

send_order(cast, upgrade, State) ->
    WaiterId = maps:get(waiter_id, State),
    OldLevel = maps:get(speed_level, State),
    NewLevel = min(OldLevel + 1, 2),
    io:format("[waiter_fsm] Waiter ~p upgraded speed from level ~p to ~p while taking order.~n", [WaiterId, OldLevel, NewLevel]),
    NewState = State#{speed_level := NewLevel},
    waiter_sup:update_waiter_state(WaiterId, NewState),
    {keep_state, NewState}.


%%%--- PICK_UP_MEAL
pick_up_meal(_Type, _Event, State) -> % Instant transition, no timeout
    WaiterId = maps:get(waiter_id, State),
    TableId = maps:get(current_table_id, State),
    Meal = maps:get(current_meal, State), % Assuming meal info is in state for serving task
    io:format("[waiter_fsm] Waiter ~p picked up meal ~p. Walking to table ~p to serve...~n", [WaiterId, Meal, TableId]),
    SpeedLevel = maps:get(speed_level, State),
    WalkTime = get_adjusted_time(walk, SpeedLevel), % Pass 'walk' as ActionType
    ServeTime = get_adjusted_time(serve, SpeedLevel), 
    TotalTime = WalkTime + ServeTime,
    {next_state, serving, State, {state_timeout, TotalTime, total_time}}.

%%%--- SERVING
serving(state_timeout, total_time, State) ->
    WaiterId = maps:get(waiter_id, State),
    TableId = maps:get(current_table_id, State),
    ClientId = maps:get(current_client_id, State),
    Meal = maps:get(current_meal, State),
    io:format("[waiter_fsm] Waiter ~p finished serving meal ~p to table ~p. Returning to idle.~n", [WaiterId, Meal, TableId]),
    gen_server:cast({global, {customer_fsm,ClientId}}, food_arrived),
    NewState = State#{current_task := undefined, current_meal := undefined, current_table_id := undefined, current_client_id := undefined},
    gen_server:cast({global, task_registry}, {waiter_ready, WaiterId}),
    {next_state, idle, NewState};

serving(cast, upgrade, State) ->
    WaiterId = maps:get(waiter_id, State),
    OldLevel = maps:get(speed_level, State),
    NewLevel = min(OldLevel + 1, 2),
    io:format("[waiter_fsm] Waiter ~p upgraded speed from level ~p to ~p while serving.~n", [WaiterId, OldLevel, NewLevel]),
    NewState = State#{speed_level := NewLevel},
    waiter_sup:update_waiter_state(WaiterId, NewState),
    {keep_state, NewState};

serving(_Type, _Event, State) ->
    {keep_state, State}.

