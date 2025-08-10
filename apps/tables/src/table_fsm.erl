%% ------------------------------------------------------------------
%% table_fsm.erl
%% Table FSM with idle / taken / dirty states and ETS reporting
%% ------------------------------------------------------------------

-module(table_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([seat_customer/2, free_table/2, clean_table/1,clean_by_player/1]).
-export([idle/3, taken/3, dirty/3]).

-define(CLEANING_TIME, 3000). %% 3 sec
-define(TABLE, table_state_ets).

-define(HEARTBEAT_INTERVAL, 5000). % 5 seconds
-export([handle_info/2]). % Add handle_info to exports

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------

start_link({TableId, Pos}) -> %create new table
    Name = {?MODULE, TableId},
     gen_statem:start_link({global, Name}, ?MODULE, {TableId, Pos}, []).

seat_customer(TableId, CustomerId) -> %send msg for customer been setead
    Name = {?MODULE, TableId},
    gen_statem:cast({global, Name}, {seat_customer, CustomerId}). 

free_table(TableId, CustomerId) -> %send msg for customer left
    Name = {?MODULE, TableId},
    gen_statem:cast({global, Name}, {free_table, CustomerId}). 

clean_table(TableId) -> %send msg for table clean
    Name = {?MODULE, TableId},
    gen_statem:cast({global, Name}, clean_table). 

clean_by_player(TableId) -> gen_statem:cast({global, {?MODULE, TableId}},clean_now).

%% ------------------------------------------------------------------
%% FSM Callbacks
%% ------------------------------------------------------------------


init({TableId, Pos}) ->
    case ets:lookup(?TABLE, TableId) of
        %% --- כאן נמצא התיקון המלא ---
        [{TableId, SavedState}] ->
            
            StateName = maps:get(state_name, SavedState, idle),
            io:format("[table_fsm] Restoring table ~p from ETS in state ~p ~n", [TableId, StateName]),
            CurrentState = SavedState#{table_id => TableId, pid => self(), table_pos => Pos, state_name => StateName},

            % 1. שלח את כל ההודעות הנדרשות כדי לסנכרן את המערכת
            notify_system_on_restore(CurrentState),

            % 2. הלוגיקה הפנימית של המצב נשארת זהה
            {ok, StateName, CurrentState};
        %% --- סוף התיקון ---

        %% התחלה חדשה (ללא שינוי)
        [] ->
            io:format("[table_fsm] Table ~p starting for the first time~n", [TableId]),
            InitialState = #{table_id => TableId, pid => self(), customer_id => undefined, state_name => idle, table_pos => Pos},
            send_heartbeat(InitialState),
            _ = table_registry:notify_table_cleaned(TableId),
            {GuiPos, _, _} = Pos,
            gen_server:cast({global, socket_server}, {send_to_gui, {add_entity, table, TableId, GuiPos, idle}}),
            {ok, idle, InitialState}
    end.



notify_system_on_restore(State) ->
    TableId   = maps:get(table_id, State),
    StateName = maps:get(state_name, State),
    {GuiPos, _, _} = maps:get(table_pos, State),

    io:format("[table_fsm] Notifying GUI (restore) table ~p in state ~p~n",
              [TableId, StateName]),
    gen_server:cast({global, socket_server},
                    {gui_update, update_state, table, TableId, StateName, GuiPos}),

    case StateName of
        idle  -> table_registry:notify_table_cleaned(TableId);
        dirty -> player:dirty_table_notification(TableId);
        taken -> ok
    end.


callback_mode() -> %fsm mood
    state_functions.

terminate(_Reason, _State, _Data) -> %otp function
    ok.

code_change(_OldVsn, State, Data, _Extra) -> %otp function
    {ok, State, Data}.

%% ------------------------------------------------------------------
%% State Definitions
%% ------------------------------------------------------------------

%% IDLE
idle(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    {keep_state, State};

idle(cast, {seat_customer, CustomerId}, State) -> %customer get to the table to seat
    TableId = maps:get(table_id, State),
    io:format("[table_fsm] Customer ~p seated at table ~p~n", [CustomerId, TableId]),
    gen_statem:cast({global, {customer_fsm, CustomerId}}, {assign_table, TableId, maps:get(table_pos, State)}), %% ←update customer about the table
    NewState = State#{customer_id => CustomerId, state_name => taken},
    table_sup:update_table_state(TableId, NewState), 
    {GuiPos, _, _} = maps:get(table_pos, NewState),
    gen_server:cast({global, socket_server},{gui_update, update_state, table, TableId, taken,GuiPos}),
    send_heartbeat(NewState),
    {next_state, taken, NewState};


%for unexpected msgs
idle(_, _, State) ->
    {keep_state, State}.


%% TAKEN
taken(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    {keep_state, State};

taken(cast, {free_table, CustomerId}, State) -> %customer got up
    TableId = maps:get(table_id, State),
    case maps:get(customer_id, State) of
        CustomerId ->
            io:format("[table_fsm] Customer ~p leaving table ~p (now dirty)~n", [CustomerId, TableId]),
            player:dirty_table_notification(TableId),
            NewState = State#{customer_id => undefined,state_name => dirty},
            table_sup:update_table_state(TableId, NewState),
            {GuiPos, _, _} = maps:get(table_pos, NewState),
            gen_server:cast({global, socket_server},{gui_update, update_state, table, TableId, dirty,GuiPos}),
            send_heartbeat(NewState),
            {next_state, dirty, NewState};
        _ ->
            io:format("[table_fsm] Unauthorized leave attempt by ~p at table ~p~n", [CustomerId, TableId]),
            {keep_state, State}
    end;

taken(cast, {free_table_timeout, CustomerId}, State) -> %customer got up
    TableId = maps:get(table_id, State),
    io:format("[table_fsm] Customer ~p leaving table ~p (now dirty)~n", [CustomerId, TableId]),
    NewState = State#{customer_id => undefined,state_name => idle},
    table_sup:update_table_state(TableId, NewState),
    table_registry:notify_table_cleaned(TableId),
    {GuiPos, _, _} = maps:get(table_pos, NewState),
    gen_server:cast({global, socket_server},{gui_update, update_state, table, TableId, idle,GuiPos}),
    send_heartbeat(NewState),
    {next_state, idle, NewState};

%for unexpected msgs
taken(_, _, State) ->
    {keep_state, State}.


%% DIRTY

dirty(info, heartbeat_tick, State) ->
    send_heartbeat(State),
    {keep_state, State};

%not used - simulation
dirty(info, clean_table_timeout, State) -> %the table is clean
    TableId = maps:get(table_id, State),
    io:format("[table_fsm] Table ~p cleaned (timeout)~n", [TableId]),
    % דווח ל-table_registry ישירות שהשולחן נקי ופנוי
    table_registry:notify_table_cleaned(TableId), % <--- שינוי קריטי!
    io:format("[table_fsm] Notified table_registry that table ~p is now cleaned and available.~n", [TableId]), % <--- הודעת דיבוג
    NewState = State#{state_name => idle},
    table_sup:update_table_state(TableId, NewState),
    {GuiPos, _, _} = maps:get(table_pos, NewState),
    gen_server:cast({global, socket_server},{gui_update, update_state, table, TableId, idle,GuiPos}),
    send_heartbeat(NewState),
    {next_state, idle, NewState};

dirty(cast, clean_now, State) ->
    TableId = maps:get(table_id, State),
    io:format("[table_fsm] Table ~p received cleaning instruction from player. Notifying registry.~n", [TableId]),
    table_registry:notify_table_cleaned(TableId),
    NewState = State#{state_name => idle},
    table_sup:update_table_state(TableId, NewState),
    {GuiPos, _, _} = maps:get(table_pos, NewState),
    gen_server:cast({global, socket_server},{gui_update, update_state, table, TableId, idle,GuiPos}),
    send_heartbeat(NewState),
    {next_state, idle, NewState};


%for unexpected msgs
dirty(_, _, State) ->
    {keep_state, State}.

% --- Helper Function to be added ---
send_heartbeat(State) ->
    table_sup:update_table_state(maps:get(table_id, State), State).

% --- Add handle_info/2 callback ---
handle_info(heartbeat_tick, State) ->
    send_heartbeat(State),
    {keep_state, State};
handle_info(_Msg, State) ->
    {keep_state, State}.