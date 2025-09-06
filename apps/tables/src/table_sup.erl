%% ------------------------------------------------------------------
%% table_sup.erl
%% Supervisor + ETS + table state manager + notify registry
%% ------------------------------------------------------------------

-module(table_sup).
-behaviour(supervisor).
-behaviour(application).

-export([
    start_link/0,
    init/1,
    start_table/1,
    notify_cleaned/1
]).
-export([ update_table_state/2]).
-export([start/2, stop/1]).
-export([start_restored_fsm/1]).

-define(ETS_UPDATE_TIME, 5000). %5sec
-define(TABLE, table_state_ets). %5sec

start_link() -> %create sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% in apps/tables/src/table_sup.erl

init([]) ->
    % Create local ETS table
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[table_sup] ETS table ~p created.~n", [?TABLE]);
        _ ->
            ok
    end,

    % Request restoration from central controller
    io:format("[table_sup] Attempting to restore state from safe_node...~n"),
    case gen_server:call({global, state_controller}, {get_full_state, tables}, 30000) of
        {ok, RestoredData} when is_list(RestoredData) ->
            if
                RestoredData =/= [] ->
                    ets:insert(?TABLE, RestoredData),
                    io:format("[table_sup] Successfully restored ~p tables from safe_node.~n", [length(RestoredData)]);
                true ->
                    io:format("[table_sup] No previous state found for tables on safe_node.~n")
            end;
        Error ->
            io:format("[table_sup] Could not restore state from safe_node: ~p~n", [Error])
    end,

    % Define permanent children 
    MngSpec = {
        table_mng,
        {table_mng, start_link, []},
        transient,
        5000,
        worker,
        [table_mng]
    },
    RegistrySpec = {
        table_registry,
        {table_registry, start_link, []},
        permanent,
        5000,
        worker,
        [table_registry]
    },

    % Restart all saved tables
    AllRestoredTables = ets:tab2list(?TABLE),
        TableChildSpecs = [
            % Capture the complete table state map
            { {table_fsm, TableId}, {table_fsm, start_link, [{TableId, maps:get(table_pos, StateMap)}]}, transient, 5000, worker, [table_fsm] }
            % Run loop on each ETS record, which has the form {TableId, StateMap}
            || {TableId, StateMap} <- AllRestoredTables
        ],

    % Assemble the final list of all children
    ChildSpecs = [MngSpec, RegistrySpec | TableChildSpecs],

    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.


start_table({TableId, Pos}) -> %create new table
    %% new fsm
    ChildId = {table_fsm, TableId},

    ChildSpec = {
        ChildId,
        {table_fsm, start_link, [{TableId, Pos}]},
        transient,
        5000,
        worker,
        [table_fsm]
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _Pid} ->
            io:format("table ~p started.~n", [TableId]),
            ok;
        {error, {already_present, _}} ->
            io:format("table ~p already exists.~n", [TableId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start table ~p: ~p~n", [TableId, Reason]),
            {error, Reason}
    end.

update_table_state(TableId, StateMap) ->
    case ets:lookup(?TABLE, TableId) of
        [{_, OldMap}] ->
            NewMap = maps:merge(OldMap, StateMap),
            ets:insert(?TABLE, {TableId, NewMap});
        [] ->
            ets:insert(?TABLE, {TableId, StateMap})
    end,
    ok.


notify_cleaned(TableId) ->
    update_table_state(TableId, #{state => idle}), 
    table_registry:notify_table_cleaned(TableId),
    ok.


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
    lists:foreach(fun({TableId, StateMap}) ->
        StateName = maps:get(state, StateMap, idle), % Default: idle
        case gen_statem:start({global, {table_fsm, TableId}}, table_fsm, 
                              {restore, TableId, StateName, StateMap}, []) of
            {ok, Pid} ->
                NewMap = maps:put(pid, Pid, StateMap),
                ets:insert(?TABLE, {TableId, NewMap}),
                io:format("[table_sup] Restored table ~p in state ~p.~n", [TableId, StateName]);
            {error, Reason} ->
                io:format("[table_sup] Failed to restore table ~p: ~p~n", [TableId, Reason])
        end
    end, StateList).

