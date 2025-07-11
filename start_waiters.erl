-module(start_waiters).
-export([start/0]).

wait_until_global(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            timer:sleep(200),  %% המתנה קצרה
            wait_until_global(Name);
        Pid ->
            io:format("[waiters] Found global ~p at ~p~n", [Name, Pid]),
            ok
    end.

start() ->
        %% המתן עד ש-state_controller ייראה
    wait_until_global(state_controller),
    application:start(waiters),

    %% התחברות לשאר ה-nodes
    net_kernel:connect_node('machines_node@127.0.0.1'),
    net_kernel:connect_node('safe_node@127.0.0.1'),
    net_kernel:connect_node('tables_node@127.0.0.1'),
    net_kernel:connect_node('customers_node@127.0.0.1'),



    %% עכשיו בטוח שאפשר להתחיל
    waiter_sup:start_waiter(waiter1),
    waiter_sup:start_waiter(waiter2),
    waiter_sup:start_waiter(waiter3),
    waiter_sup:start_waiter(waiter4).
