-module(task_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([request_task/1, notify_waiter_available/1, cancel_task/1, get_state/0]).
-export([add_waiter/1, remove_waiter/1]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{task_queue => queue:new(), available_waiters => []}}.

% --- Public API ---

request_task(Task) ->
    gen_server:cast({global, ?MODULE}, {request_task, Task}).

notify_waiter_available(WaiterId) ->
    gen_server:cast({global, ?MODULE}, {notify_waiter_available, WaiterId}).

cancel_task(TaskId) ->
    gen_server:cast({global, ?MODULE}, {cancel_task, TaskId}).

get_state() ->
    gen_server:call({global, ?MODULE}, get_state_sync).

add_waiter(WaiterId) ->
    gen_server:cast({global, ?MODULE}, {add_waiter, WaiterId}).

remove_waiter(WaiterId) ->
    gen_server:cast({global, ?MODULE}, {remove_waiter, WaiterId}).

% --- gen_server callbacks ---

handle_cast({request_task, Task}, State) ->
    AvailableWaiters = maps:get(available_waiters, State),
    Queue = maps:get(task_queue, State),
    
    io:format("[task_registry] Received task request: ~p. Available waiters: ~p, Queue length: ~p~n", 
              [Task, AvailableWaiters, queue:len(Queue)]),
    
    case AvailableWaiters of
        [WaiterId | Rest] ->
            % Assign task to available waiter (simplified - just log for now)
            io:format("[task_registry] Assigning task ~p to waiter ~p~n", [Task, WaiterId]),
            % TODO: When you add waiter_fsm, call: waiter_fsm:assign_task(WaiterId, Task)
            {noreply, State#{available_waiters => Rest}};
        [] ->
            % Add task to queue
            NewQueue = queue:in(Task, Queue),
            io:format("[task_registry] No available waiters, task ~p added to queue. New queue length: ~p~n", 
                      [Task, queue:len(NewQueue)]),
            {noreply, State#{task_queue => NewQueue}}
    end;

handle_cast({notify_waiter_available, WaiterId}, State) ->
    Queue = maps:get(task_queue, State),
    AvailableWaiters = maps:get(available_waiters, State),
    
    io:format("[task_registry] Waiter ~p is available. Current queue length: ~p~n", 
              [WaiterId, queue:len(Queue)]),
    
    case queue:out(Queue) of
        {{value, Task}, NewQueue} ->
            % Assign task to available waiter (simplified - just log for now)
            io:format("[task_registry] Assigning queued task ~p to waiter ~p. Remaining queue: ~p~n", 
                      [Task, WaiterId, queue:len(NewQueue)]),
            % TODO: When you add waiter_fsm, call: waiter_fsm:assign_task(WaiterId, Task)
            {noreply, State#{task_queue => NewQueue}};
        {empty, _} ->
            % Add waiter to available list
            NewAvailableWaiters = [WaiterId | AvailableWaiters],
            io:format("[task_registry] No tasks in queue, waiter ~p added to queue. Available waiters: ~p~n", 
                      [WaiterId, NewAvailableWaiters]),
            {noreply, State#{available_waiters => NewAvailableWaiters}}
    end;

handle_cast({cancel_task, TaskId}, State) ->
    Queue = maps:get(task_queue, State),
    NewQueue = queue:filter(fun(Task) -> 
        case Task of
            #{task_id := Id} -> Id =/= TaskId;
            _ -> true
        end
    end, Queue),
    io:format("[task_registry] Cancelled task ~p. Queue length: ~p~n", [TaskId, queue:len(NewQueue)]),
    {noreply, State#{task_queue => NewQueue}};

handle_cast({add_waiter, WaiterId}, State) ->
    AvailableWaiters = maps:get(available_waiters, State),
    Queue = maps:get(task_queue, State),
    
    case lists:member(WaiterId, AvailableWaiters) of
        true ->
            io:format("[task_registry] Waiter ~p is already in available list~n", [WaiterId]),
            {noreply, State};
        false ->
            % Check if there are tasks in queue
            case queue:out(Queue) of
                {{value, Task}, NewQueue} ->
                    % Assign task to available waiter
                    io:format("[task_registry] Assigning queued task ~p to waiter ~p. Remaining queue: ~p~n", 
                              [Task, WaiterId, queue:len(NewQueue)]),
                    % TODO: When you add waiter_fsm, call: waiter_fsm:assign_task(WaiterId, Task)
                    {noreply, State#{task_queue => NewQueue}};
                {empty, _} ->
                    % No tasks in queue, add waiter to available list
                    NewAvailableWaiters = [WaiterId | AvailableWaiters],
                    io:format("[task_registry] No tasks in queue, waiter ~p added to available list. Available waiters: ~p~n", 
                              [WaiterId, NewAvailableWaiters]),
                    {noreply, State#{available_waiters => NewAvailableWaiters}}
            end
    end;

handle_cast({remove_waiter, WaiterId}, State) ->
    AvailableWaiters = maps:get(available_waiters, State),
    case lists:member(WaiterId, AvailableWaiters) of
        true ->
            NewAvailableWaiters = lists:delete(WaiterId, AvailableWaiters),
            io:format("[task_registry] Removed waiter ~p from available list. Available waiters: ~p~n", [WaiterId, NewAvailableWaiters]),
            {noreply, State#{available_waiters => NewAvailableWaiters}};
        false ->
            io:format("[task_registry] Waiter ~p is not in available list~n", [WaiterId]),
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