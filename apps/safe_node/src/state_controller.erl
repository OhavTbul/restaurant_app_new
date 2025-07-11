-module(state_controller).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3, handle_info/2]).

% ETS table names
-define(WAITERS_TABLE, safe_waiters_ets).
-define(CUSTOMERS_TABLE, safe_customers_ets).
-define(MACHINES_TABLE, safe_machines_ets).
-define(TABLES_TABLE, safe_tables_ets).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    % יצירת טבלאות ETS נפרדות לכל סוג ישות
    ets:new(?WAITERS_TABLE, [named_table, public, set]),
    ets:new(?CUSTOMERS_TABLE, [named_table, public, set]),
    ets:new(?MACHINES_TABLE, [named_table, public, set]),
    ets:new(?TABLES_TABLE, [named_table, public, set]),
    io:format("[state_controller] All safe ETS tables created.~n"),
    {ok, #{}}.

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

% in state_controller.erl

handle_info({nodedown, Node}, State) ->
    io:format("!!! NODE DOWN DETECTED: ~p !!!~n", [Node]),
    % חילוץ שם האפליקציה מתוך שם ה-node (למשל 'waiters_node@...' -> waiters)
    AppStr = atom_to_list(Node),
    [NameStr | _] = string:split(AppStr, "_node"),
    AppName = list_to_atom(NameStr),


    case AppName of
        Type when Type == waiters; Type == customers; Type == machines; Type == tables ->
            io:format("Spawning restorer for ~p...~n", [Type]),
            spawn(fun() -> restorer:restore_node(Type) end);

        _ ->
            io:format("Unknown node ~p. No restorer started.~n", [AppName])
    end,

    {noreply, State};

handle_info({nodeup, Node}, State) ->
    io:format("--- NODE UP DETECTED: ~p ---~n", [Node]),
    % כאן נוסיף לוגיקה אם נרצה לטפל בחזרה של node
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.