-module(machine_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([machine_order/2, upgrade/1]).

% States
-export([idle/3, cooking/3]).

-define(HEARTBEAT_INTERVAL, 5000). % 5 seconds
-export([handle_info/2]). % Add handle_info to exports

%%%=======================
%%% Public API
%%%=======================

start_link(MachineId) -> %create machine
    Name = {?MODULE, MachineId},
    gen_statem:start_link({global, Name}, ?MODULE, MachineId, []).

machine_order(MachineId, Order) -> %sendin msg to create order
    Name = {?MODULE, MachineId},
    gen_statem:cast({global, Name}, {machine_order, Order}).

upgrade(MachineId) -> %sending msg to upgrade
    Name = {?MODULE, MachineId},
    gen_statem:cast({global, Name}, upgrade).

%%%=======================
%%% State Functions
%%%=======================

init(MachineId) ->
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    case ets:lookup(machine_state, MachineId) of
        [{_, SavedState}] ->
            io:format("Restoring machine ~p from ETS~n", [MachineId]),
            State = SavedState#{machine_id => MachineId, pid => self(), state_name => maps:get(state_name, SavedState, idle)},
            StateName = maps:get(state_name, SavedState, idle),
            case StateName of
                cooking ->
                    Order = maps:get(current_order, State),
                    Delay = machine_time(maps:get(upgrade_level, State, 0)),
                    timer:send_after(Delay, {cooking_done, Order}),
                    {ok, cooking, State};
                idle ->
                    {ok, idle, State}
            end;
        [] ->
            %% First time startup
            io:format("Machine ~p starting (idle).~n", [MachineId]),
            State = #{
                machine_id => MachineId, 
                pid => self(), 
                current_order => undefined, 
                upgrade_level => 0,
                state_name => idle
            },
            
            % *** התיקון הקריטי כאן: שמור את המצב המלא מיד עם ההתחלה ***
            send_heartbeat(State),

            gen_server:cast({global, order_registry}, {machine_ready, MachineId}),
            {ok, idle, State}
    end.


callback_mode() -> %fsm mood
    state_functions.

terminate(_Reason, _StateName, _State) -> %opt function
    ok.

code_change(_OldVsn, StateName, State, _Extra) -> %opt function
    {ok, StateName, State}.

%%%=======================
%%% Time per upgrade level
%%%=======================

machine_time(0) -> 3000;  % 3sec
machine_time(1) -> 2000;  % 2sec
machine_time(2) -> 1000;  % 1sec
machine_time(_) -> 1000.  % 1sec

%%%=======================
%%% IDLE State
%%%=======================
idle(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

idle(cast, {machine_order, Order}, State) ->
    Upgrade = maps:get(upgrade_level, State, 0),
    Delay = machine_time(Upgrade),
    timer:send_after(Delay, {cooking_done, Order}), %send when done cooking
    NewState = State#{current_order := Order},
    machine_sup:update_machine_state(maps:get(machine_id, NewState), NewState),
    {next_state, cooking, NewState};

idle(cast, upgrade, State) ->
    Old = maps:get(upgrade_level, State, 0),
    New = min(Old + 1, 2),
    io:format("Machine ~p upgraded to level ~p~n", [maps:get(machine_id, State), New]),
    NewState = State#{upgrade_level := New},
    machine_sup:update_machine_state(maps:get(machine_id, NewState), NewState),
    {keep_state, NewState};

%for unexpeced msgs
idle(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

%%%=======================
%%% cooking State
%%%=======================
cooking(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};

cooking(info, {cooking_done, Order}, State) ->
    io:format("Machine ~p finished making order: ~p~n", [maps:get(machine_id, State), Order]),
    NewState = State#{current_order := undefined,state_name => idle},
    machine_sup:update_machine_state(maps:get(machine_id, NewState), NewState),
    % Add delivery task to waiter queue
    case Order of
        #{table_id := TableId, client_id := CustomerId, meal := Meal} ->
            % V-- התיקון --V
            Task = #{
                type => serve_meal, 
                table_id => TableId, 
                client_id => CustomerId, 
                meal => Meal
            },
            gen_server:cast({global, task_registry}, {add_task, Task}),
            io:format("[machine_fsm] Added 'serve_meal' task to waiter queue for customer ~p at table ~p~n", [CustomerId, TableId]);
        _ ->
            io:format("[machine_fsm] Invalid order format or missing details in order: ~p~n", [Order])
    end,
    gen_server:cast({global, order_registry}, {machine_ready, maps:get(machine_id, State)}),
    {next_state, idle, NewState};

cooking(cast, upgrade, State) ->
    % Allow upgrade during processing; will affect next order
    Old = maps:get(upgrade_level, State, 0),
    New = min(Old + 1, 2),
    io:format("Machine ~p upgraded during work to level ~p~n", [maps:get(machine_id, State), New]),
    NewState = State#{upgrade_level := New},
    {keep_state, NewState};

%for unexpeced msgs
cooking(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

%%%=======================
%%% Generic Handler
%%%=======================
%%%
send_heartbeat(State) ->
    machine_sup:update_machine_state(maps:get(machine_id, State), State).

handle_event(EventType, EventContent, State) ->
    io:format("Machine ~p received unexpected event ~p: ~p~n", [maps:get(machine_id, State), EventType, EventContent]),
    {keep_state, State}.

% --- Add handle_info/2 callback ---
handle_info(heartbeat_tick, State) ->
    send_heartbeat(State),
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat_tick),
    {keep_state, State};
handle_info(Msg, State) ->
    io:format("[machine_fsm] Unexpected info: ~p~n", [Msg]),
    {keep_state, State}.

