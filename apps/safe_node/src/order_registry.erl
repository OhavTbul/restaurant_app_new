-module(order_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([machine_ready/1, get_state/0]).
-export([remove_machine/1,add_order/1]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{order_queue => queue:new(), available_machine => [],order_counter => 0}}.

% --- Public API ---

machine_ready(MachineId) ->
    gen_server:cast({global, ?MODULE}, {machine_ready, MachineId}). %machine ready to get order


get_state() ->
    gen_server:call({global, ?MODULE}, get_state_sync).

remove_machine(MachineId) ->
    gen_server:cast({global, ?MODULE}, {remove_machine, MachineId}).

add_order(Order) ->
    gen_server:cast({global, ?MODULE}, {add_order, Order}).

% --- gen_server callbacks ---
handle_cast({machine_ready, MachineId}, State) ->
    Queue = maps:get(order_queue, State),
    AvailableMachines = maps:get(available_machine, State),
    io:format("[order_registry] machine ~p is available to get a order. Order queue length: ~p~n", [MachineId, queue:len(Queue)]),
    case queue:out(Queue) of
        {{value, OrderWithId}, NewQueue} -> %% <-- שים לב לשם המשתנה
            io:format("[order_registry] Assigning order ~p to machine ~p~n", [OrderWithId, MachineId]),
            gen_statem:cast({global, {machine_fsm, MachineId}}, {machine_order, OrderWithId}),
            {noreply, State#{order_queue => NewQueue}};
        {empty, _} ->
            case lists:member(MachineId, AvailableMachines) of
                true ->
                    io:format("[order_registry] machine ~p already in available list~n", [MachineId]),
                    {noreply, State};
                false ->
                    NewAvailableMachines = [MachineId | AvailableMachines],
                    io:format("[order_registry] No orders available. machine ~p added to available list~n", 
                              [MachineId]),
                    {noreply, State#{available_machine => NewAvailableMachines}}
            end
    end;


handle_cast({add_order, Order}, State) ->
    Queue = maps:get(order_queue, State),
    AvailableMachines = maps:get(available_machine, State),
    OrderCounter = maps:get(order_counter, State),
    OrderWithId = maps:put(order_id, OrderCounter, Order),

    io:format("[order_registry] Received new order: ~p. Available machines: ~p~n", [OrderWithId, AvailableMachines]),
    case AvailableMachines of
        [MachineId | RestMachines] ->
            io:format("[order_registry] Assigning order ~p to machine ~p~n", [OrderWithId, MachineId]),
            gen_statem:cast({global, {machine_fsm, MachineId}}, {machine_order, OrderWithId}),
            {noreply, State#{
                available_machine => RestMachines,
                order_counter => OrderCounter + 1
            }};
        [] ->
            NewQueue = queue:in(OrderWithId, Queue),
            io:format("[order_registry] No available machines. order ~p added to queue. New queue length: ~p~n",
                      [OrderWithId, queue:len(NewQueue)]),
            {noreply, State#{
                order_queue => NewQueue,
                order_counter => OrderCounter + 1
            }}
    end;



handle_cast({remove_machine, MachineId}, State) ->
    AvailableMachines = maps:get(available_machine, State),
    case lists:member(MachineId, AvailableMachines) of
        true ->
            NewAvailableMachines = lists:delete(MachineId, AvailableMachines),
            io:format("[order_registry] Removed machine ~p from available list. Available machines: ~p~n", [MachineId, NewAvailableMachines]),
            {noreply, State#{available_machine => NewAvailableMachines}};
        false ->
            io:format("[order_registry] machine ~p is not in available list~n", [MachineId]),
            {noreply, State}
    end.

handle_call(get_state_sync, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}. 