-module(player).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    add_table/0,
    upgrade_waiter/1, add_waiter/0,
    upgrade_machine/1, add_machine/0,
    get_prices/0, get_waiters/0,
    show_tables/0, show_waiters/0, show_machines/0,start_game/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    do_add_table/1, do_add_waiter/1, 
    do_upgrade_machine/1, do_add_machine/1, increase_price/2,
    do_upgrade_specific_waiter/2, do_upgrade_specific_machine/2,
    dirty_table_notification/1,clean_dirty_table/1
]).


-record(prices, {
    add_table = 80,
    upgrade_waiter = 70,
    add_waiter = 100,
    upgrade_machine = 90,
    add_machine = 120
}).

-record(state, {
    table_counter = 0,
    waiter_counter = 0,
    machine_counter = 0,
    prices = #prices{},
    dirty_tables = [],
    table_pos = #{},  
    machine_pos = #{}
}).

%%% ================================
%%% API
%%% ================================

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

add_table()         -> gen_server:call(?MODULE, add_table).
upgrade_waiter(WaiterId) -> gen_server:call(?MODULE, {upgrade_waiter, WaiterId}).
add_waiter()        -> gen_server:call(?MODULE, add_waiter).
upgrade_machine(MachineId) -> gen_server:call(?MODULE, {upgrade_machine, MachineId}).
add_machine()       -> gen_server:call(?MODULE, add_machine).
get_prices()        -> gen_server:call(?MODULE, get_prices).
get_waiters()       -> gen_server:call(?MODULE, get_waiters).
show_tables()       -> gen_server:call(?MODULE, show_tables).
show_waiters()      -> gen_server:call(?MODULE, show_waiters).
show_machines()     -> gen_server:call(?MODULE, show_machines).
dirty_table_notification(TableId) -> gen_server:cast({global, ?MODULE}, {dirty_table, TableId}).
clean_dirty_table(TableId) ->
    gen_server:call({global, ?MODULE}, {clean_dirty_table, TableId}).

start_game() -> gen_server:call(?MODULE, rquest_start_game).


%%% ================================
%%% list_to_map
%%% ================================

list_to_map([], _Counter, Map) ->
    Map;
list_to_map([Coord | Rest], Counter, Map) ->
    list_to_map(Rest, Counter + 1, Map#{Counter => Coord}).

%%% ================================
%%% gen_server callbacks
%%% ================================

init([]) ->
    io:format("start player~n", []),

    % נניח שאנחנו רוצים למקם שולחנות החל מ־Row 5 ו־Col 5
    RowStart = 5,
    ColStart = 5,
    RowGap = 3, % כל שולחן תופס 2×2 (לכן קפיצה של 3)
    ColGap = 3,

    % מספר שורות וטורים של שולחנות (לדוגמה 10×15)
    NumRows = 10,
    NumCols = 15,

    % בונה את כל המיקומים
    TableCoords = generate_table_coords(RowStart, ColStart, RowGap, ColGap, NumRows, NumCols, 1, []),

    Tmap = maps:from_list(TableCoords),

    % מכונות באזור עליון
    MachineCoords  = [ {Row, Col} || Row <- lists:seq(0, 4), Col <- lists:seq(40, 49) ],
    Mmap = list_to_map(MachineCoords , 1, #{}),

    {ok, #state{table_pos = Tmap, machine_pos = Mmap}}.

% פונקציה רקורסיבית ליצירת רשימת מיקומים
generate_table_coords(_, _, _, _, 0, _, _, Acc) -> Acc;
generate_table_coords(RowStart, ColStart, RowGap, ColGap, RowCount, ColCount, ID, Acc) ->
    Row = RowStart + RowGap * (10 - RowCount), % כל שורה בקפיצה
    {NewAcc, NewID} = generate_table_row(Row, ColStart, ColGap, ColCount, ID, Acc),
    generate_table_coords(RowStart, ColStart, RowGap, ColGap, RowCount - 1, ColCount, NewID, NewAcc).

generate_table_row(_, _, _, 0, ID, Acc) -> {Acc, ID};
generate_table_row(Row, ColStart, ColGap, ColCount, ID, Acc) ->
    Col = ColStart + ColGap * (15 - ColCount),
    TablePos = {Row, Col},                        % ימנית עליונה
    CustomerPos = {Row, Col - 1},                 % שמאלית עליונה
    WaiterPos = {Row + 1, Col - 1},               % שמאלית תחתונה
    Entry = {ID, {TablePos, CustomerPos, WaiterPos}},
    generate_table_row(Row, ColStart, ColGap, ColCount - 1, ID + 1, [Entry | Acc]).


handle_call(rquest_start_game, _From, State) ->
    io:format("Player starting customers application remotely...~n"),
    
    % מנסה להתחיל את היישום על צומת הלקוחות
    Result = rpc:call('customers_node@127.0.0.1', application, start, [customers]),
    
    % בודק האם הקריאה נכשלה
    if
        Result =/= ok ->
            io:format("ERROR: Failed to start customers application: ~p~n", [Result]),
            {reply, {error, failed_to_start_customers}, State};
        true ->
            % אם הצליח, ממשיך לשאר הלוגיקה
            Tcount = State#state.table_counter,
            Wcount = State#state.waiter_counter,
            Mcount = State#state.machine_counter,
            Table1 = list_to_atom("table_" ++ integer_to_list(Tcount + 1)),
            PosT1 = maps:get(Tcount + 1, State#state.table_pos),
            PosT2 = maps:get(Tcount + 2, State#state.table_pos),
            PosM1 = maps:get(Mcount + 1, State#state.machine_pos),
            Table2 = list_to_atom("table_" ++ integer_to_list(Tcount + 2)),
            Mchine1 = list_to_atom("machine_" ++ integer_to_list(Mcount + 1)),
            Waiter1 = list_to_atom("waiter_" ++ integer_to_list(Wcount + 1)),
            
            case gen_server:call({global, table_mng}, {start_table, {Table1, PosT1}}) of
                ok ->
                    io:format("Table ~p added successfully~n", [Table1]),
                    State1 = State#state{table_counter = Tcount + 1},
                    case gen_server:call({global, table_mng}, {start_table, {Table2, PosT2}}) of
                        ok ->
                            io:format("Table ~p added successfully~n", [Table2]),
                            State2 = State1#state{table_counter = Tcount + 2},
                            case gen_server:call({global, waiter_mng}, {start_waiter, Waiter1}) of
                                ok ->
                                    io:format("waiter ~p added successfully~n", [Waiter1]),
                                    State3 = State2#state{waiter_counter = Wcount + 1},
                                    case gen_server:call({global, machine_mng}, {start_cook, {Mchine1, PosM1}}) of
                                        ok ->
                                            io:format("machine ~p added successfully~n", [Mchine1]),
                                            State4 = State3#state{machine_counter = Mcount + 1},
                                            {reply, ok, State4};
                                        {error, Reason} ->
                                            {reply, {error, Reason}, State3}
                                    end;
                                {error, Reason} ->
                                    {reply, {error, Reason}, State2}
                            end;
                        {error, Reason} ->
                            {reply, {error, Reason}, State1}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;


handle_call(add_table, _From, State) ->
    Prices = State#state.prices,
    handle_purchase(Prices#prices.add_table, do_add_table, State);

handle_call({upgrade_waiter, WaiterId}, _From, State) ->
    Prices = State#state.prices,
    handle_purchase(Prices#prices.upgrade_waiter, {do_upgrade_specific_waiter, WaiterId}, State);

handle_call(add_waiter, _From, State) ->
    Prices = State#state.prices,
    handle_purchase(Prices#prices.add_waiter, do_add_waiter, State);

handle_call({upgrade_machine, MachineId}, _From, State) ->
    Prices = State#state.prices,
    handle_purchase(Prices#prices.upgrade_machine, {do_upgrade_specific_machine, MachineId}, State);

handle_call(add_machine, _From, State) ->
    Prices = State#state.prices,
    handle_purchase(Prices#prices.add_machine, do_add_machine, State);

handle_call(get_prices, _From, State) ->
    {reply, State#state.prices, State};

handle_call({get_price, Type}, _From, State = #state{prices = Prices}) ->
    Price = case Type of
        add_table -> Prices#prices.add_table;
        upgrade_waiter -> Prices#prices.upgrade_waiter;
        add_waiter -> Prices#prices.add_waiter;
        upgrade_machine -> Prices#prices.upgrade_machine;
        add_machine -> Prices#prices.add_machine;
        _ -> 0
    end,
    {reply, Price, State};


handle_call(get_waiters, _From, State) ->
    {reply, State#state.waiter_counter, State};

handle_call(show_tables, _From, State) ->
    Tables = ets:tab2list(table_state_ets),
    {reply, Tables, State};

handle_call(show_waiters, _From, State) ->
    Waiters = ets:tab2list(waiter_state),
    {reply, Waiters, State};

handle_call(show_machines, _From, State) ->
    Machines = ets:tab2list(machine_state),
    {reply, Machines, State};

handle_call({clean_dirty_table, TableId}, _From, State = #state{dirty_tables = DirtyTables}) ->
    CleanPrice = 20, 
    case lists:member(TableId, DirtyTables) of
        true ->
            case cashier:insert_money(CleanPrice) of
                {ok, _NewBalance}->
                    io:format("Player insert ~p on cleaning table ~p.~n", [CleanPrice, TableId]),
                    table_fsm:clean_by_player(TableId),
                    NewDirtyTables = lists:delete(TableId, DirtyTables),
                    {reply, ok, State#state{dirty_tables = NewDirtyTables}};
                {error, Reason} ->
                    io:format("Player cannot clean table ~p: ~p~n", [TableId, Reason]),
                    {reply, {error, Reason}, State}
            end;
        false ->
            io:format("Table ~p is not dirty or already being cleaned.~n", [TableId]),
            {reply, {error, not_dirty}, State}
    end;




handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.


handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.

%%% ================================
%%% Internal Helpers
%%% ================================

handle_purchase(Price, Action, State) ->
    case cashier:spend_money(Price) of
        ok ->
            % You can send message to other processes or spawn actions here
            NewState = case Action of
                {ActionName, Param} ->
                    apply(?MODULE, ActionName, [Param, State]);
                ActionName ->
                    apply(?MODULE, ActionName, [State])
            end,
            % Increase the price by 20 after successful purchase
            UpdatedState = increase_price(Action, NewState),
            {reply, ok, UpdatedState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

%%% Dummy upgrade functions — replace with actual logic later

increase_price(do_add_table, State) ->
    Prices = State#state.prices,
    NewPrices = Prices#prices{add_table = Prices#prices.add_table + 20},
    State#state{prices = NewPrices};

increase_price({do_upgrade_specific_waiter, _WaiterId}, State) ->
    Prices = State#state.prices,
    NewPrices = Prices#prices{upgrade_waiter = Prices#prices.upgrade_waiter + 20},
    State#state{prices = NewPrices};

increase_price(do_add_waiter, State) ->
    Prices = State#state.prices,
    NewPrices = Prices#prices{add_waiter = Prices#prices.add_waiter + 20},
    State#state{prices = NewPrices};

increase_price(do_upgrade_machine, State) ->
    Prices = State#state.prices,
    NewPrices = Prices#prices{upgrade_machine = Prices#prices.upgrade_machine + 20},
    State#state{prices = NewPrices};

increase_price(do_add_machine, State) ->
    Prices = State#state.prices,
    NewPrices = Prices#prices{add_machine = Prices#prices.add_machine + 20},
    State#state{prices = NewPrices};

increase_price({do_upgrade_specific_machine, _MachineId}, State) ->
    Prices = State#state.prices,
    NewPrices = Prices#prices{upgrade_machine = Prices#prices.upgrade_machine + 20},
    State#state{prices = NewPrices}.

do_add_table(State = #state{table_counter = Counter}) ->
    TableId = list_to_atom("table_" ++ integer_to_list(Counter + 1)),
    PosT = maps:get(Counter + 1, State#state.table_pos),
    case gen_server:call({global, table_mng}, {start_table, {TableId, PosT}}) of
        ok ->
            io:format("Table ~p added successfully~n", [TableId]),
            State#state{table_counter = Counter + 1};
        {error, Reason} ->
            io:format("Failed to add table ~p: ~p~n", [TableId, Reason]),
            State
    end.

do_upgrade_specific_waiter(WaiterId, State) ->
    case gen_server:call({global, waiter_mng}, {upgrade_waiter, WaiterId}) of
        ok ->
            io:format("Waiter ~p upgraded successfully~n", [WaiterId]),
            State;
        {error, Reason} ->
            io:format("Failed to upgrade waiter ~p: ~p~n", [WaiterId, Reason]),
            State
    end.

do_add_waiter(State = #state{waiter_counter = Counter}) ->
    WaiterId = list_to_atom("waiter_" ++ integer_to_list(Counter + 1)),
    case gen_server:call({global, waiter_mng}, {start_waiter, WaiterId}) of
        ok ->
            io:format("Waiter ~p added successfully~n", [WaiterId]),
            State#state{waiter_counter = Counter + 1};
            
        {error, Reason} ->
            io:format("Failed to add waiter ~p: ~p~n", [WaiterId, Reason]),
            State
    end.

do_upgrade_machine(State) -> 
    % For now, we'll need to specify which machine to upgrade
    % This could be enhanced to upgrade the most recently added machine
    % or to track which machines exist
    io:format("Machine upgrade - specify machine ID~n"), 
    State.

do_upgrade_specific_machine(MachineId, State) ->
    case gen_server:call({global, machine_mng}, {upgrade_machine, MachineId}) of
        ok ->
            io:format("Machine ~p upgraded successfully~n", [MachineId]),
            State;
        {error, Reason} ->
            io:format("Failed to upgrade machine ~p: ~p~n", [MachineId, Reason]),
            State
    end.

do_add_machine(State = #state{machine_counter = Counter}) ->
    MachineId = list_to_atom("machine_" ++ integer_to_list(Counter + 1)),
    PosM = maps:get(Counter + 1, State#state.machine_pos),
    case gen_server:call({global, machine_mng}, {start_cook, {MachineId, PosM}}) of
        ok ->
            io:format("Machine ~p added successfully~n", [MachineId]),
            State#state{machine_counter = Counter + 1};
            
        {error, Reason} ->
            io:format("Failed to add machine ~p: ~p~n", [MachineId, Reason]),
            State
    end.


handle_cast({dirty_table, TableId}, State = #state{dirty_tables = DirtyTables}) ->
    io:format("Player received notification that table ~p is dirty.~n", [TableId]),
    NewDirtyTables = lists:usort([TableId | DirtyTables]),
    {noreply, State#state{dirty_tables = NewDirtyTables}};

handle_cast(_Msg, State) ->
    {noreply, State}.

