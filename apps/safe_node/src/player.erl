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
    dirty_table_notification/1,clean_dirty_table/1,
    create_tables/3, create_waiters/3, create_machines/3
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

    % Updated positioning for column-based layout
    % Start tables at row 5, column 5 to avoid kitchen area
    RowStart = 5,
    ColStart = 5,
    RowGap = 4,  % Increased gap between rows for better spacing
    ColGap = 6,  % Increased gap between columns to prevent overlap

    % Number of rows and columns for table layout
    % With 7 tables per column, we can have up to 17 columns (120/7 ≈ 17)
    NumRows = 10,   % Each column has 7 tables
    NumCols = 17,  % Maximum number of columns

    % Build all table positions using the new column-based system
    TableCoords = generate_table_coords(RowStart, ColStart, RowGap, ColGap, NumRows, NumCols, 1, []),

    Tmap = maps:from_list(TableCoords),

    % Machines in upper area (kitchen)
    MachineCoords  = [ {Row, Col} || Row <- lists:seq(0, 4), Col <- lists:seq(40, 49) ],
    Mmap = list_to_map(MachineCoords , 1, #{}),

    {ok, #state{table_pos = Tmap, machine_pos = Mmap}}.

% Recursive function to create list of positions

% Helper function to create multiple tables
create_tables(State, StartCount, NumTables) ->
    create_tables_recursive(State, StartCount, NumTables, 0).

create_tables_recursive(State, _StartCount, NumTables, NumTables) ->
    State;
create_tables_recursive(State, StartCount, NumTables, Created) ->
    CurrentCount = StartCount + Created + 1,
    TableId = list_to_atom("table_" ++ integer_to_list(CurrentCount)),
    Pos = maps:get(CurrentCount, State#state.table_pos),
    
    case gen_server:call({global, table_mng}, {start_table, {TableId, Pos}}) of
        ok ->
            io:format("Table ~p added successfully~n", [TableId]),
            NewState = State#state{table_counter = CurrentCount},
            create_tables_recursive(NewState, StartCount, NumTables, Created + 1);
        {error, Reason} ->
            io:format("Failed to create table ~p: ~p~n", [TableId, Reason]),
            State
    end.

% Helper function to create multiple waiters
create_waiters(State, StartCount, NumWaiters) ->
    create_waiters_recursive(State, StartCount, NumWaiters, 0).

create_waiters_recursive(State, _StartCount, NumWaiters, NumWaiters) ->
    State;
create_waiters_recursive(State, StartCount, NumWaiters, Created) ->
    CurrentCount = StartCount + Created + 1,
    WaiterId = list_to_atom("waiter_" ++ integer_to_list(CurrentCount)),
    
    case gen_server:call({global, waiter_mng}, {start_waiter, WaiterId}) of
        ok ->
            io:format("Waiter ~p added successfully~n", [WaiterId]),
            NewState = State#state{waiter_counter = CurrentCount},
            create_waiters_recursive(NewState, StartCount, NumWaiters, Created + 1);
        {error, Reason} ->
            io:format("Failed to create waiter ~p: ~p~n", [WaiterId, Reason]),
            State
    end.

% Helper function to create multiple machines
create_machines(State, StartCount, NumMachines) ->
    create_machines_recursive(State, StartCount, NumMachines, 0).

create_machines_recursive(State, _StartCount, NumMachines, NumMachines) ->
    State;
create_machines_recursive(State, StartCount, NumMachines, Created) ->
    CurrentCount = StartCount + Created + 1,
    MachineId = list_to_atom("machine_" ++ integer_to_list(CurrentCount)),
    Pos = maps:get(CurrentCount, State#state.machine_pos),
    
    case gen_server:call({global, machine_mng}, {start_cook, {MachineId, Pos}}) of
        ok ->
            io:format("Machine ~p added successfully~n", [MachineId]),
            NewState = State#state{machine_counter = CurrentCount},
            create_machines_recursive(NewState, StartCount, NumMachines, Created + 1);
        {error, Reason} ->
            io:format("Failed to create machine ~p: ~p~n", [MachineId, Reason]),
            State
    end.

% New column-based table positioning system
% Each column will have 7 tables, and new columns are created after the 7th table
generate_table_coords(RowStart, ColStart, RowGap, ColGap, NumRows, NumCols, ID, Acc) ->
    io:format("Generating table coordinates with column-based layout~n"),
    io:format("RowStart: ~p, ColStart: ~p, RowGap: ~p, ColGap: ~p~n", [RowStart, ColStart, RowGap, ColGap]),
    Result = generate_table_coords_by_columns(RowStart, ColStart, RowGap, ColGap, NumRows, NumCols, ID, Acc, 1),
    io:format("Generated ~p table positions~n", [length(Result)]),
    Result.

% Generate tables column by column, with 7 tables per column
generate_table_coords_by_columns(_, _, _, _, _, _, ID, Acc, _) when ID > 120 -> 
    io:format("Reached maximum table limit (120)~n"),
    Acc;
generate_table_coords_by_columns(RowStart, ColStart, RowGap, ColGap, NumRows, NumCols, ID, Acc, Column) ->
    % Calculate column position (each column is separated by ColGap)
    Col = ColStart + (Column - 1) * ColGap,
    io:format("Generating column ~p at position ~p~n", [Column, Col]),
    
    % Generate 7 tables for this column
    {NewAcc, NewID} = generate_tables_in_column(RowStart, Col, RowGap, ID, Acc, 1, 7),
    
    % Continue with next column if we have more tables
    generate_table_coords_by_columns(RowStart, ColStart, RowGap, ColGap, NumRows, NumCols, NewID, NewAcc, Column + 1).

% Generate tables within a single column
generate_tables_in_column(_, _, _, ID, Acc, RowNum, MaxRows) when RowNum > MaxRows -> 
    io:format("Completed column with ~p tables, next ID: ~p~n", [MaxRows, ID]),
    {Acc, ID};
generate_tables_in_column(RowStart, Col, RowGap, ID, Acc, RowNum, MaxRows) ->
    % Calculate row position within the column
    Row = RowStart + (RowNum - 1) * RowGap,
    
    % Create table positions for this table
    TablePos = {Row, Col},                        % Table position
    CustomerPos = {Row, Col - 1},                 % Customer position (left of table)
    WaiterPos = {Row + 1, Col - 1},               % Waiter position (below customer)
    
    % Create entry for this table
    Entry = {ID, {TablePos, CustomerPos, WaiterPos}},
    io:format("Table ~p: Table(~p,~p), Customer(~p,~p), Waiter(~p,~p)~n", 
              [ID, Row, Col, Row, Col-1, Row+1, Col-1]),
    
    % Continue with next row in this column
    generate_tables_in_column(RowStart, Col, RowGap, ID + 1, [Entry | Acc], RowNum + 1, MaxRows).


handle_call(rquest_start_game, _From, State) ->
    io:format("Player starting customers application remotely...~n"),
    
    % Try to start the application on the customers node
    Result = rpc:call('customers_node@127.0.0.1', application, start, [customers]),
    
    % Check if the call failed
    if
        Result =/= ok ->
            io:format("ERROR: Failed to start customers application: ~p~n", [Result]),
            {reply, {error, failed_to_start_customers}, State};
        true ->
            % If successful, continue with the rest of the logic
            Tcount = State#state.table_counter,
            Wcount = State#state.waiter_counter,
            Mcount = State#state.machine_counter,
            
            % Create 5 tables
            State1 = create_tables(State, Tcount, 5),
            
            % Create 3 waiters
            State2 = create_waiters(State1, Wcount, 3),
            
            % Create 2 machines
            State3 = create_machines(State2, Mcount, 2),
            
            {reply, ok, State3}
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

