-module(task_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([waiter_ready/1, cancel_task/1, get_state/0]).
-export([remove_waiter/1,add_task/1]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{task_queue => queue:new(), available_waiters => [],task_counter => 0}}.

% --- Public API ---

waiter_ready(WaiterId) ->
    gen_server:cast({global, ?MODULE}, {waiter_ready, WaiterId}). %waiter ready to get task

cancel_task(TaskId) ->
    gen_server:cast({global, ?MODULE}, {cancel_task, TaskId}).

get_state() ->
    gen_server:call({global, ?MODULE}, get_state_sync).

remove_waiter(WaiterId) ->
    gen_server:cast({global, ?MODULE}, {remove_waiter, WaiterId}).

add_task(Task) ->
    gen_server:cast({global, ?MODULE}, {add_task, Task}).

% --- gen_server callbacks ---

handle_cast({waiter_ready, WaiterId}, State) ->
    Queue = maps:get(task_queue, State),
    AvailableWaiters = maps:get(available_waiters, State),

    io:format("[task_registry] Waiter ~p is available to get a task. Task queue length: ~p~n", [WaiterId, queue:len(Queue)]),

    case queue:out(Queue) of
        {{value, Task}, NewQueue} ->
            io:format("[task_registry] Assigning task ~p to waiter ~p~n", [Task, WaiterId]),
            gen_statem:cast({global, {waiter_fsm, WaiterId}}, {task, Task}),
            {noreply, State#{task_queue => NewQueue}};
        {empty, _} ->
            %% No task — add the waiter to available list (if not already)
            case lists:member(WaiterId, AvailableWaiters) of
                true ->
                    io:format("[task_registry] Waiter ~p already in available list~n", [WaiterId]),
                    {noreply, State};
                false ->
                    NewAvailableWaiters = [WaiterId | AvailableWaiters],
                    io:format("[task_registry] No tasks available. Waiter ~p added to available list~n", 
                              [WaiterId]),
                    {noreply, State#{available_waiters => NewAvailableWaiters}}
            end
    end;

handle_cast({add_task, Task}, State) ->
    Queue = maps:get(task_queue, State),
    AvailableWaiters = maps:get(available_waiters, State),
    TaskCounter = maps:get(task_counter, State),
    TaskWithId = maps:put(task_id, TaskCounter, Task),

    io:format("[task_registry] Received new task: ~p. Available waiters: ~p~n", [TaskWithId, AvailableWaiters]),
    case AvailableWaiters of
        [WaiterId | RestWaiters] ->
            io:format("[task_registry] Assigning task ~p to waiter ~p~n", [TaskWithId, WaiterId]),
            gen_statem:cast({global, {waiter_fsm, WaiterId}}, {task, TaskWithId}),
            {noreply, State#{
                available_waiters => RestWaiters,
                task_counter => TaskCounter + 1
            }};
        [] ->
            NewQueue = queue:in(TaskWithId, Queue),
            io:format("[task_registry] No available waiters. Task ~p added to queue. New queue length: ~p~n",
                      [TaskWithId, queue:len(NewQueue)]),
            {noreply, State#{
                task_queue => NewQueue,
                task_counter => TaskCounter + 1
            }}
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