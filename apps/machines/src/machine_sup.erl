-module(machine_sup).
-behaviour(supervisor).
-behaviour(application).

-export([
    start_link/0,
    init/1,
    start_cook/1,
    upgrade_machine/1,
    update_machine_state/2,
    handle_info/2
]).

-define(TABLE, machine_state).
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
            io:format("[machine_sup] ETS table ~p created.~n", [?TABLE]);
        _ ->
            ok % Table already exists, do nothing
    end,

    % בקשת שחזור מהבקר המרכזי
    io:format("[machine_sup] Attempting to restore state from safe_node...~n"),
    case gen_server:call({global, state_controller}, {get_full_state, machines}, 30000) of
        {ok, RestoredData} when is_list(RestoredData) ->
            if
                RestoredData =/= [] ->
                    ets:insert(?TABLE, RestoredData),
                    io:format("[machine_sup] Successfully restored ~p machines from safe_node.~n", [length(RestoredData)]);
                true ->
                    io:format("[machine_sup] No previous state found for machines on safe_node.~n")
            end;
        Error ->
            io:format("[machine_sup] Could not restore state from safe_node: ~p~n", [Error])
    end,

    % הפעלת המנהל
    MngSpec = {
        machine_mng,
        {machine_mng, start_link, []},
        transient, 
        5000,
        worker,
        [machine_mng]
    },
    
    % הפעלה מחדש של כל המכונות שנשמרו
    AllRestoredMachines = ets:tab2list(?TABLE),
    ChildSpecs = [
        % קולט את המפה המלאה של מצב המכונה
        { {machine_fsm, MachineId}, {machine_fsm, start_link, [{MachineId, maps:get(pos, StateMap)}]}, transient, 5000, worker, [machine_fsm] }
        % מריץ לולאה על כל רשומת ETS, שצורתה {MachineId, StateMap}
        || {MachineId, StateMap} <- AllRestoredMachines
    ],

    {ok, {{one_for_one, 5, 10}, [MngSpec | ChildSpecs]}}.



%%%===================
%%% create new machine
%%%===================

start_cook({MachineId, Pos}) ->
    io:format("[machine_sup] creating machine ~p~n", [MachineId]),
    %% new fsm
    ChildId = {machine_fsm, MachineId},

    ChildSpec = {
        ChildId,                                    
        {machine_fsm, start_link, [{MachineId, Pos}]},     
        transient,                                  
        5000,                                      
        worker,                                     
        [machine_fsm]                               
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _Pid} ->
            io:format("Machine ~p started.~n", [MachineId]),
            ok;
        {error, {already_present, _}} ->
            io:format("Machine ~p already exists.~n", [MachineId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start machine ~p: ~p~n", [MachineId, Reason]),
            {error, Reason}
    end.

%%%===================
%%% upgrade machine
%%%===================

upgrade_machine(MachineId) ->
    case ets:lookup(?TABLE, MachineId) of
        [{MachineId, _MachineState}] ->
            machine_fsm:upgrade(MachineId),
            ok;
        [] ->
            io:format("Cannot upgrade. Machine ~p not found.~n", [MachineId]),
            {error, not_found}
    end.

%%%===================
%%% update mechine
%%%===================

% in apps/machines/src/machine_sup.erl

update_machine_state(MachineId, Data) ->
    case ets:lookup(?TABLE, MachineId) of
        [{_, OldMap}] ->
            NewMap = maps:merge(OldMap, Data),
            ets:insert(?TABLE, {MachineId, NewMap});
        [] ->
            ets:insert(?TABLE, {MachineId, Data})
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
    lists:foreach(fun({MachineId, StateMap}) ->
        StateName = maps:get(state, StateMap, idle),
        case gen_statem:start({global, {machine_fsm, MachineId}}, machine_fsm,
                              {restore, MachineId, StateName, StateMap}, []) of
            {ok, Pid} ->
                NewMap = maps:put(pid, Pid, StateMap),
                ets:insert(?TABLE, {MachineId, NewMap}),
                io:format("[machine_sup] Restored machine ~p in state ~p.~n", [MachineId, StateName]);
            {error, Reason} ->
                io:format("[machine_sup] Failed to restore machine ~p: ~p~n", [MachineId, Reason])
        end
    end, StateList).

