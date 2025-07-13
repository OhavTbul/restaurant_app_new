-module(waiter_sup).
-behaviour(supervisor).
-behaviour(application).

-export([
    start_link/0,
    init/1,
    start_waiter/1,
    upgrade_waiter/1,
    update_waiter_state/2,
    handle_info/2
]).

-define(TABLE, waiter_state).
-define(REPORT_INTERVAL, 5000). %5sec
-export([start/2, stop/1]).
-export([start_restored_fsm/1]).

%%%===================
%%% API
%%%===================

start_link() -> %create sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % יצירת טבלת ETS מקומית
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[waiter_sup] ETS table ~p created.~n", [?TABLE]);
        _ ->
            ok % Table already exists, do nothing
    end,

    % בקשת שחזור מהבקר המרכזי
    io:format("[waiter_sup] Attempting to restore state from safe_node...~n"),
    case gen_server:call({global, state_controller}, {get_full_state, waiters}, 30000) of
        {ok, RestoredData} when is_list(RestoredData) ->
            if
                RestoredData =/= [] -> %if there is data in the controller
                    ets:insert(?TABLE, RestoredData),
                    io:format("[waiter_sup] Successfully restored ~p waiters from safe_node.~n", [length(RestoredData)]);
                true ->
                    io:format("[waiter_sup] No previous state found for waiters on safe_node.~n")
            end;
        Error ->
            io:format("[waiter_sup] Could not restore state from safe_node: ~p~n", [Error])
    end,

    % הפעלת הילדים 
    MngSpec = {
        waiter_mng,
        {waiter_mng, start_link, []},
        transient, 
        5000,
        worker,
        [waiter_mng]
    },
    
    % הפעלה מחדש של כל המלצרים שנשמרו
    AllRestoredWaiters = ets:tab2list(?TABLE),
    ChildSpecs = [
        { {waiter_fsm, WaiterId}, {waiter_fsm, start_link, [WaiterId]}, transient, 5000, worker, [waiter_fsm] }
        || {WaiterId, _} <- AllRestoredWaiters
    ],

    {ok, {{one_for_one, 5, 10}, [MngSpec | ChildSpecs]}}.

%%%===================
%%% create new machine
%%%===================

start_waiter(WaiterId) ->
    %% new fsm
    ChildId = {waiter_fsm, WaiterId},


    ChildSpec = {
        ChildId,                                    
        {waiter_fsm, start_link, [WaiterId]},     
        transient,                                  
        5000,                                      
        worker,                                     
        [waiter_fsm]                               
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _Pid} ->
            io:format("waiter ~p started.~n", [WaiterId]),
            ok;
        {error, {already_present, _}} ->
            io:format("waiter ~p already exists.~n", [WaiterId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start waiter ~p: ~p~n", [WaiterId, Reason]),
            {error, Reason}
    end.

%%%===================
%%% upgrade waiter
%%%===================

upgrade_waiter(WaiterId) ->
    case ets:lookup(?TABLE, WaiterId) of
        [{WaiterId, _WaiterState}] ->
            waiter_fsm:upgrade(WaiterId),
            ok;
        [] ->
            io:format("Cannot upgrade. waiter ~p not found.~n", [WaiterId]),
            {error, not_found}
    end.

%%%===================
%%% update waiter
%%%===================

update_waiter_state(WaiterId, Data) ->
    case ets:lookup(?TABLE, WaiterId) of
        [{_, OldMap}] ->
            NewMap = maps:merge(OldMap, Data),
            ets:insert(?TABLE, {WaiterId, NewMap});
        [] ->
            ets:insert(?TABLE, {WaiterId, Data})
    end,
    ok.


%%%===================
%%% Handle info
%%%===================
%%%
%for unexpected msgs
handle_info(_, State) ->
    {noreply, State}.


%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(_StartType, _StartArgs) ->
    ?MODULE:start_link().


stop(_State) ->
    ok.
%%%===================================================================
%%% restore
%%%===================================================================
start_restored_fsm(StateList) ->
    lists:foreach(fun({WaiterId, StateMap}) ->
        case supervisor:start_child(?MODULE, {
            {waiter_fsm, WaiterId},
            {waiter_fsm, start_link, [WaiterId, StateMap]},
            transient,
            5000,
            worker,
            [waiter_fsm]
        }) of
            {ok, Pid} ->
                NewMap = maps:put(pid, Pid, StateMap),
                ets:insert(?TABLE, {WaiterId, NewMap}),
                io:format("[waiter_sup] Restored waiter ~p.~n", [WaiterId]);
            {error, {already_present, _}} ->
                io:format("[waiter_sup] Waiter ~p already running.~n", [WaiterId]);
            {error, Reason} ->
                io:format("[waiter_sup] Failed to restore waiter ~p: ~p~n", [WaiterId, Reason])
        end
    end, StateList).





