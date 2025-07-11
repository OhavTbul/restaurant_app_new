-module(restorer).
-export([restore_node/1]).

restore_node(waiters) ->
    case ets:tab2list(safe_waiters_ets) of
        [] ->
            io:format("[restorer] No saved waiters state found.~n"),
            ok;
        StateList ->
            io:format("[restorer] Restoring ~p waiters from ETS...~n", [length(StateList)]),
            case supervisor:start_child(safe_node_sup, {waiter_sup, {waiter_sup, start_link, []}, permanent, 5000, supervisor, [waiter_sup]}) of
                {ok, _Pid} ->
                    % הקריאה ל-waiter_sup:start_restored_fsm הוסרה כאן.
                    % המפקח כבר מפעיל את הילדים בעצמו.
                    io:format("[restorer] Successfully started waiter supervisor.~n");
                {error, Reason} ->
                    io:format("[restorer] Failed to start waiter_sup: ~p~n", [Reason])
            end
    end;

restore_node(customers) ->
    case ets:tab2list(safe_customers_ets) of
        [] ->
            io:format("[restorer] No saved customers state found.~n"),
            ok;
        StateList ->
            io:format("[restorer] Restoring ~p customers from ETS...~n", [length(StateList)]),
            case supervisor:start_child(safe_node_sup, {customer_sup, {customer_sup, start_link, []}, permanent, 5000, supervisor, [customer_sup]}) of
                {ok, _Pid} ->
                    % הקריאה ל-customer_sup:start_restored_fsm הוסרה.
                    io:format("[restorer] Successfully started customer supervisor.~n");
                {error, Reason} ->
                    io:format("[restorer] Failed to start customer_sup: ~p~n", [Reason])
            end
    end;

restore_node(machines) ->
    case ets:tab2list(safe_machines_ets) of
        [] ->
            io:format("[restorer] No saved machines state found.~n"),
            ok;
        StateList ->
            io:format("[restorer] Restoring ~p machines from ETS...~n", [length(StateList)]),
            case supervisor:start_child(safe_node_sup, {machine_sup, {machine_sup, start_link, []}, permanent, 5000, supervisor, [machine_sup]}) of
                {ok, _Pid} ->
                    % הקריאה ל-machine_sup:start_restored_fsm הוסרה.
                    io:format("[restorer] Successfully started machine supervisor.~n");
                {error, Reason} ->
                    io:format("[restorer] Failed to start machine_sup: ~p~n", [Reason])
            end
    end;

restore_node(tables) ->
    case ets:tab2list(safe_tables_ets) of
        [] ->
            io:format("[restorer] No saved tables state found.~n"),
            ok;
        StateList ->
            io:format("[restorer] Restoring ~p tables from ETS...~n", [length(StateList)]),
            case supervisor:start_child(safe_node_sup, {table_sup, {table_sup, start_link, []}, permanent, 5000, supervisor, [table_sup]}) of
                {ok, _Pid} ->
                    % הקריאה ל-table_sup:start_restored_fsm הוסרה.
                    io:format("[restorer] Successfully started table supervisor.~n");
                {error, Reason} ->
                    io:format("[restorer] Failed to start table_sup: ~p~n", [Reason])
            end
    end;

restore_node(_) ->
    io:format("[restorer] Unknown node type for restore.~n"),
    error.