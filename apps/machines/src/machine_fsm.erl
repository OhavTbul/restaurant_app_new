-module(machine_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([machine_order/2, upgrade/1]).

% States
-export([idle/3, cooking/3]).



%%%=======================
%%% Public API
%%%=======================

start_link(MachineId) -> %create machine
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    gen_statem:start_link({global, Name}, ?MODULE, MachineId, []).

machine_order(MachineId, Order) -> %sendin msg to create order
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    gen_statem:cast({global, Name}, {machine_order, Order}).

upgrade(MachineId) -> %sending msg to upgrade
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    gen_statem:cast({global, Name}, upgrade).

%%%=======================
%%% State Functions
%%%=======================

init(MachineId) ->
    %% Try to restore from ETS
    case ets:lookup(machine_state, MachineId) of
        [{_, SavedState}] ->
            io:format("Restoring machine ~p from ETS~n", [MachineId]),
            State = SavedState#{machine_id => MachineId},
            case maps:get(current_order, SavedState, undefined) of
                undefined ->
                    {ok, idle, State};
                Order ->
                    Delay = machine_time(maps:get(upgrade_level, State, 0)),
                    timer:send_after(Delay, {cooking_done, Order}),
                    {ok, cooking, State}
            end;
        [] ->
            %% First time startup
            {ok, idle, #{machine_id => MachineId, current_order => undefined, upgrade_level => 0}}
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

idle(cast, {machine_order, Order}, State) ->
    Upgrade = maps:get(upgrade_level, State, 0),
    Delay = machine_time(Upgrade),
    timer:send_after(Delay, {cooking_done, Order}), %send when done cooking
    NewState = State#{current_order := Order},
    machine_sup:update_machine_state(maps:get(machine_id, NewState), #{
        state => cooking,
        position => {200, 300}, %todo check positions
        upgrade_level => Upgrade,
        current_order => Order
    }),
    {next_state, cooking, NewState};

idle(cast, upgrade, State) ->
    Old = maps:get(upgrade_level, State, 0),
    New = min(Old + 1, 2),
    io:format("Machine ~p upgraded to level ~p~n", [maps:get(machine_id, State), New]),
    NewState = State#{upgrade_level := New},
    machine_sup:update_machine_state(maps:get(machine_id, State), #{
        state => idle,
        position => {200, 300}, %todo check positions
        upgrade_level => New,
        current_order => undefined
    }),
    {keep_state, NewState};

%for unexpeced msgs
idle(EventType, EventContent, State) ->
    handle_event(EventType, EventContent, State).

%%%=======================
%%% cooking State
%%%=======================

cooking(info, {cooking_done, Order}, State) ->
    io:format("Machine ~p finished making order: ~p~n", [maps:get(machine_id, State), Order]),
    NewState = State#{current_order := undefined},
    machine_sup:update_machine_state(maps:get(machine_id, State), #{
        state => idle,
        position => {200, 300}, %todo check positions
        upgrade_level => maps:get(upgrade_level, State),
        current_order => undefined
    }),
    % Add delivery task to waiter queue
    case Order of
        #{table_id := TableId, client_id := CustomerId} ->
            Task = {deliver_food, TableId, CustomerId, Order},
            task_registry:request_task(Task),
            io:format("[machine_fsm] Added delivery task to waiter queue for customer ~p at table ~p~n", [CustomerId, TableId]);
        _ ->
            io:format("[machine_fsm] Invalid order format or missing customer_id in order: ~p~n", [Order])
    end,
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

handle_event(EventType, EventContent, State) ->
    io:format("Machine ~p received unexpected event ~p: ~p~n", [maps:get(machine_id, State), EventType, EventContent]),
    {keep_state, State}.






