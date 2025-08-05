-module(state_controller).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_cast/2, handle_call/3, handle_info/2]).

% ETS table names
-define(WAITERS_TABLE, safe_waiters_ets).
-define(CUSTOMERS_TABLE, safe_customers_ets).
-define(MACHINES_TABLE, safe_machines_ets).
-define(TABLES_TABLE, safe_tables_ets).

-record(state, {
    % Maps an entity type (e.g., waiters) to the original node that handled it.
    % This map is our 'source of truth' and doesn't change after init.
    workload_map :: map(),

    % Maps an ACTIVE node to a LIST of entity types it's currently responsible for.
    % This map is dynamic and changes on node failures.
    responsibilities :: map()
}).

start_link(Nodes) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, Nodes, []).

init(Nodes) ->
    net_kernel:monitor_nodes(true),
    rand:seed(exsplus),
    % יצירת טבלאות ETS נפרדות לכל סוג ישות
    ets:new(?WAITERS_TABLE, [named_table, public, set]),
    ets:new(?CUSTOMERS_TABLE, [named_table, public, set]),
    ets:new(?MACHINES_TABLE, [named_table, public, set]),
    ets:new(?TABLES_TABLE, [named_table, public, set]),
    io:format("[state_controller] All safe ETS tables created.~n"),
    WorkloadMap = maps:from_list([
        {list_to_atom(hd(string:split(atom_to_list(Node), "_node"))), Node}
     || Node <- Nodes
    ]),
    % At the beginning, each node is responsible for its own entity type
    Responsibilities = maps:from_list([
        {Node, [EntityType]}
     || {EntityType, Node} <- maps:to_list(WorkloadMap)
    ]),
    io:format("[state_controller] Initial responsibilities mapped: ~p~n", [Responsibilities]),
    {ok, #state{workload_map = WorkloadMap, responsibilities = Responsibilities}}.

% כאן נטפל בהודעות העדכון
handle_cast({update, EntityType, DataList}, State) ->
    SafeTable = case EntityType of
        waiters -> ?WAITERS_TABLE;
        customers -> ?CUSTOMERS_TABLE;
        machines -> ?MACHINES_TABLE;
        tables -> ?TABLES_TABLE
    end,
    % מחיקת המידע הישן והכנסת המידע החדש
    ets:delete_all_objects(SafeTable),
    ets:insert(SafeTable, DataList),
    io:format("[state_controller] Updated state for ~p with ~p records.~n", [EntityType, length(DataList)]),
    {noreply, State};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

% כאן נטפל בבקשות שחזור
handle_call({get_full_state, EntityType}, _From, State) ->
    SafeTable = case EntityType of
        waiters -> ?WAITERS_TABLE;
        customers -> ?CUSTOMERS_TABLE;
        machines -> ?MACHINES_TABLE;
        tables -> ?TABLES_TABLE
    end,
    FullData = ets:tab2list(SafeTable),
    {reply, {ok, FullData}, State};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.


%% NEW: The core logic for handling a node failure with delegation.
handle_info({nodedown, FailedNode}, State = #state{workload_map = Workloads, responsibilities = Resp}) ->
    io:format("!!! NODE DOWN DETECTED: ~p !!!~n", [FailedNode]),

    case maps:is_key(FailedNode, Resp) of
        true ->
            % 1. Find which entity types the failed node was responsible for.
            OrphanedEntityTypes = maps:get(FailedNode, Resp),
            io:format("Node ~p was responsible for: ~p~n", [FailedNode, OrphanedEntityTypes]),

            % 2. Find a live node to take over.
            % erlang:nodes() gives all other live nodes.
            Candidates = erlang:nodes(),

            case Candidates of
                [_H | _T] -> % If there are other nodes alive
                    % 3. Choose a random "heir" from the candidates
                    HeirNode = lists:nth(rand:uniform(length(Candidates)), Candidates),
                    io:format("Choosing heir node ~p to take over.~n", [HeirNode]),
                    % השורה החדשה והנכונה:
                    {value, HeirEntityType} = lists:search(fun({_Type, Node}) -> Node == HeirNode end, maps:to_list(Workloads)),
                    {HeirOriginalEntityType, _} = HeirEntityType,                    
                    HeirManagerName = case HeirOriginalEntityType of
                        customers -> customer_mng;
                        waiters -> waiter_mng;
                        machines -> machine_mng;
                        tables -> table_mng
                    end,
                    DestinationProcess = {global, HeirManagerName},
                    Message = {take_over_responsibilities, OrphanedEntityTypes},
                    io:format("Casting message ~p to manager ~p~n", [Message, DestinationProcess]),
                    gen_server:cast(DestinationProcess, Message),

                    % 5. Update responsibility
                    NewResp = maps:remove(FailedNode, Resp),
                    HeirOldResp = maps:get(HeirNode, NewResp, []), % Get heir's old tasks, or [] if none
                    HeirNewResp = HeirOldResp ++ OrphanedEntityTypes,
                    FinalResp = maps:put(HeirNode, HeirNewResp, NewResp),
                    io:format("New responsibilities: ~p~n", [FinalResp]),
                    {noreply, State#state{responsibilities = FinalResp}};

                [] -> % אין מועמדים, ה-state_controller לוקח אחריות
                    io:format("CRITICAL: No other candidate nodes. This node (~p) is taking over responsibility.~n", [node()]),

                    % --- כאן אנחנו משלבים את הלוגיקה הישנה שלך ---
                    % אנחנו מפעילים לולאה על כל אחריות ברשימה שהוצאנו
                    lists:foreach(
                      fun(EntityType) ->
                          % זה הלב של הקוד הישן שלך, עכשיו הוא רץ לכל אחריות
                          io:format("Spawning restorer for ~p...~n", [EntityType]),
                          spawn(fun() -> restorer:restore_node(EntityType) end)
                      end,
                      OrphanedEntityTypes % הרשימה שאנחנו עוברים עליה
                    ),

                    % עדכון מפת האחריות, בדיוק כמו שעשינו קודם
                    NewResp = maps:remove(FailedNode, Resp),
                    MyOldResp = maps:get(node(), NewResp, []),
                    MyNewResp = MyOldResp ++ OrphanedEntityTypes,
                    FinalResp = maps:put(node(), MyNewResp, NewResp),
                    io:format("New responsibilities: ~p~n", [FinalResp]),
                    {noreply, State#state{responsibilities = FinalResp}}
            end;
        false ->
            % This can happen if a node that wasn't in our list goes down.
            io:format("Node ~p went down, but was not in the responsibility list. Ignoring.~n", [FailedNode]),
            {noreply, State}
    end;


handle_info({nodeup, Node}, State) ->
    io:format("--- NODE UP DETECTED: ~p ---~n", [Node]),
    % כאן נוסיף לוגיקה אם נרצה לטפל בחזרה של node
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.