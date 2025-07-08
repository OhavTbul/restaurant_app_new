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

-define(ETS_UPDATE_TIME, 5000). %5sec
-define(TABLE, table_state_ets). %5sec

start_link() -> %create sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[table_sup] ETS table ~p created.~n", [?TABLE]);
        _ ->
            ok % Table already exists, do nothing
    end,
    %maneger procces - communicate with safe node
    MngSpec = {
        table_mng,
        {table_mng, start_link, []},
        transient,
        5000,
        worker,
        [table_mng]
    },
       %% הוספת ה-registry כילד של המפקח
    RegistrySpec = {
        table_registry,
        {table_registry, start_link, []},
        permanent, % הרישום חייב לרוץ תמיד
        5000,
        worker,
        [table_registry]
    },

    {ok, {{one_for_one, 5, 10}, [MngSpec, RegistrySpec]}}.


start_table(TableId) -> %create new table
    %% new fsm
    ChildId = list_to_atom("table_fsm" ++ atom_to_list(TableId)),

    ChildSpec = {
        ChildId,
        {table_fsm, start_link, [TableId]},
        transient,
        5000,
        worker,
        [table_fsm]
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            ets:insert(?TABLE, {TableId, #{pid => Pid}}),
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