-module(customer_sup).
-behaviour(supervisor).
-behaviour(application).

-export([start_link/0, init/1, start_client/1, update_customer_state/2]).
-export([handle_info/2]).

-define(TABLE_STATE, customer_state).
-define(ETS_UPDATE_TIME, 5000). % 5sec between ETS updates


-export([start/2, stop/1]).
-export([start_restored_fsm/1]).

%%%===================
%%% API
%%%===================

start_link() -> %start sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("[customer_sup] init/1 started~n"),
    % יצירת טבלת ETS מקומית
    case ets:info(?TABLE_STATE) of
        undefined ->
            ets:new(?TABLE_STATE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[customer_sup] ETS table ~p created.~n", [?TABLE_STATE]);
        _ ->
            ok
    end,

    % בקשת שחזור מהבקר המרכזי
    io:format("[customer_sup] Attempting to restore state from safe_node...~n"),
    {ok, RestoredData} = case gen_server:call({global, state_controller}, {get_full_state, customers}, 30000) of
        {ok, Data} when is_list(Data) ->
            if
                Data =/= [] ->
                    ets:insert(?TABLE_STATE, Data),
                    io:format("[customer_sup] Successfully restored ~p customers from safe_node.~n", [length(Data)]);
                true ->
                    io:format("[customer_sup] No previous state found for customers on safe_node.~n")
            end,
            {ok, Data};
        Error ->
            io:format("[customer_sup] Could not restore state from safe_node: ~p~n", [Error]),
            {ok, []} % במקרה של שגיאה, נמשיך עם רשימה ריקה
    end,

    % הגדרת ילד המנהל, עם המידע המשוחזר
    MngSpec = {
        customer_mng,
        {customer_mng, start_link, [RestoredData]}, % העברת הרשימה כארגומנט
        transient, 
        5000,
        worker,
        [customer_mng]
    },

    %הפעלה מחדש של כל הלקוחות שנשמרו
    ChildSpecs = [
        { {customer_fsm, CustomerId}, {customer_fsm, start_link, [CustomerId]}, transient, 5000, worker, [customer_fsm] }
        || {CustomerId, _} <- RestoredData
    ],
    
    io:format("[customer_sup] Returning supervisor spec~n"),
    {ok, {{one_for_one, 5, 10}, [MngSpec | ChildSpecs]}}.

%%%===================
%%% Start & Restart Customers
%%%===================

start_client(CustomerId) ->
    %% new fsm
     ChildId = {customer_fsm, CustomerId},

    ChildSpec = {
        ChildId,                                    
        {customer_fsm, start_link, [CustomerId]},     
        transient,                                  
        5000,                                      
        worker,                                     
        [customer_fsm]                               
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _Pid} ->
            io:format("customer ~p started.~n", [CustomerId]),
            ok;
        {error, {already_present, _}} ->
            io:format("customer ~p already exists.~n", [CustomerId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start customer ~p: ~p~n", [CustomerId, Reason]),
            {error, Reason}
    end.



%%%===================
%%% Customer sends updated state periodically
%%%===================

update_customer_state(CustomerId, Data) ->
    case ets:lookup(?TABLE_STATE, CustomerId) of
        [{_, OldMap}] ->
            NewMap = maps:merge(OldMap, Data),
            ets:insert(?TABLE_STATE, {CustomerId, NewMap});
        [] ->
            ets:insert(?TABLE_STATE, {CustomerId, Data})
    end,
    ok.

%%%===================
%%% Handle customer crash and restart
%%%===================


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
    lists:foreach(fun({CustomerId, StateMap}) ->
        StateName = maps:get(state, StateMap, idle),
        case gen_statem:start({global, {customer_fsm, CustomerId}}, customer_fsm,
                              {restore, CustomerId, StateName, StateMap}, []) of
            {ok, Pid} ->
                NewMap = maps:put(pid, Pid, StateMap),
                ets:insert(?TABLE_STATE, {CustomerId, NewMap}),
                io:format("[customer_sup] Restored customer ~p in state ~p.~n", [CustomerId, StateName]);
            {error, Reason} ->
                io:format("[customer_sup] Failed to restore customer ~p: ~p~n", [CustomerId, Reason])
        end
    end, StateList).

