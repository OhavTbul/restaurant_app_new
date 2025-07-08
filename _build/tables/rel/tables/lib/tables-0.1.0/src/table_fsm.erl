%% ------------------------------------------------------------------
%% table_fsm.erl
%% Table FSM with idle / taken / dirty states and ETS reporting
%% ------------------------------------------------------------------

-module(table_fsm).
-behaviour(gen_statem).

-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).
-export([seat_customer/2, free_table/2, clean_table/1]).
-export([idle/3, taken/3, dirty/3]).

-define(CLEANING_TIME, 3000). %% 3 sec
-define(TABLE, table_state_ets).

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------

start_link(TableId) -> %create new table
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
     gen_statem:start_link({global, Name}, ?MODULE, TableId, []).

seat_customer(TableId, CustomerId) -> %send msg for customer been setead
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:cast({global, Name}, {seat_customer, CustomerId}). 

free_table(TableId, CustomerId) -> %send msg for customer left
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:cast({global, Name}, {free_table, CustomerId}). 

clean_table(TableId) -> %send msg for table clean
    Name = list_to_atom("table_fsm_" ++ atom_to_list(TableId)),
    gen_statem:cast({global, Name}, clean_table). 

%% ------------------------------------------------------------------
%% FSM Callbacks
%% ------------------------------------------------------------------


init(TableId) ->
    case ets:lookup(?TABLE, TableId) of
        %% שחזור מצב
        [{TableId, SavedState}] ->
            io:format("[table_fsm] Restoring table ~p from ETS with state: ~p~n", [TableId, maps:get(state, SavedState, "unknown")]),
            CurrentState = SavedState#{table_id => TableId}, 
            StateName = maps:get(state, SavedState, idle),

            %% הפעלה מחדש של הטיימר המתאים למצב
            case StateName of
                dirty ->
                    {ok, dirty, CurrentState, {state_timeout, ?CLEANING_TIME, clean_table_timeout}};
                taken ->
                    % כאן אין טיימר, אז זה תקין
                    {ok, taken, CurrentState};
                idle ->
                    % כאן אין טיימר, אז זה תקין
                    {ok, idle, CurrentState}
            end;

        %% התחלה חדשה
        [] ->
            io:format("[table_fsm] Table ~p starting for the first time~n", [TableId]),
            InitialState = #{ % <--- וודא שההזחה נכונה
                table_id => TableId,
                customer_id => undefined,
                state => idle
            },
            % זהו השינוי הקריטי: קריאה סינכרונית ל-table_registry
            % וודא שכל השורות הבאות מועתקות בדיוק, כולל הפסיקים והנקודה-פסיק
            Result = case table_registry:notify_table_cleaned(TableId) of
                ok -> io:format("[table_fsm] Successfully notified table_registry about new table ~p (sync).~n", [TableId]), ok;
                {error, Reason} -> io:format("[table_fsm] ERROR: Failed to notify table_registry about new table ~p: ~p~n", [TableId, Reason]), {error, Reason}
            end, % <--- וודא שיש פסיק בסוף ה-end אם יש עוד ביטויים אחריו
            io:format("[table_fsm] Notified table_registry that table ~p is available.~n", [TableId]), % <--- וודא שיש פסיק בסוף השורה
            {ok, idle, InitialState}
    end. % <--- וודא שיש רק end. אחד כאן


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

idle(cast, {seat_customer, CustomerId}, State) -> %customer get to the table to seat
    TableId = maps:get(table_id, State),
    io:format("[table_fsm] Customer ~p seated at table ~p~n", [CustomerId, TableId]),
     gen_statem:cast({global, {customer_fsm, CustomerId}}, {assign_table, TableId}), %% ←update customer about the table
    NewState = State#{customer_id => CustomerId},
    table_sup:update_table_state(TableId, #{state => taken, customer_id => CustomerId}),
    {next_state, taken, NewState};


%for unexpected msgs
idle(_, _, State) ->
    {keep_state, State}.


%% TAKEN

taken(cast, {free_table, CustomerId}, State) -> %customer got up
    TableId = maps:get(table_id, State),
    case maps:get(customer_id, State) of
        CustomerId ->
            io:format("[table_fsm] Customer ~p leaving table ~p (now dirty)~n", [CustomerId, TableId]),
            erlang:send_after(?CLEANING_TIME, self(), clean_table_timeout), %simulation - cleaned alone todo - change
            NewState = State#{customer_id => undefined},
            table_sup:update_table_state(TableId, #{state => dirty}),
            {next_state, dirty, NewState};
        _ ->
            io:format("[table_fsm] Unauthorized leave attempt by ~p at table ~p~n", [CustomerId, TableId]),
            {keep_state, State}
    end;

%for unexpected msgs
taken(_, _, State) ->
    {keep_state, State}.


%% DIRTY


dirty(info, clean_table_timeout, State) -> %the table is clean
    TableId = maps:get(table_id, State),
    io:format("[table_fsm] Table ~p cleaned (timeout)~n", [TableId]),
    % דווח ל-table_registry ישירות שהשולחן נקי ופנוי
    table_registry:notify_table_cleaned(TableId), % <--- שינוי קריטי!
    io:format("[table_fsm] Notified table_registry that table ~p is now cleaned and available.~n", [TableId]), % <--- הודעת דיבוג
    table_sup:update_table_state(TableId, #{state => idle}),
    {next_state, idle, State};


%for unexpected msgs
dirty(_, _, State) ->
    {keep_state, State}.