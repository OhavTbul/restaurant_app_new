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

%%%===================
%%% API
%%%===================

start_link() -> %create sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[machine_sup] ETS table ~p created.~n", [?TABLE]);
        _ ->
            ok % Table already exists, do nothing
    end,
    %maneger procces - communicate with safe node
    MngSpec = {
        machine_mng,
        {machine_mng, start_link, []},
        transient, 
        5000,
        worker,
        [machine_mng]
    },
    %%Define child spec - customer_fsm - simple child that can be duplicate

    {ok, {{one_for_one, 5, 10}, [MngSpec]}}.

%%%===================
%%% create new machine
%%%===================

start_cook(MachineId) ->
    %% new fsm
    ChildId = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),

    ChildSpec = {
        ChildId,                                    
        {machine_fsm, start_link, [MachineId]},     
        transient,                                  
        5000,                                      
        worker,                                     
        [machine_fsm]                               
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            ets:insert(?TABLE, {MachineId, #{pid => Pid, upgrade_level => 0}}),
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
    Name = list_to_atom("machine_fsm_" ++ atom_to_list(MachineId)),
    case whereis(Name) of
        undefined ->
            io:format("Cannot upgrade. Machine ~p not found.~n", [MachineId]),
            {error, not_found};
        _ ->
            machine_fsm:upgrade(MachineId),
            ok
    end.

%%%===================
%%% update mechine
%%%===================

update_machine_state(MachineId, Data) ->
    ets:insert(?TABLE, {MachineId, Data}),
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