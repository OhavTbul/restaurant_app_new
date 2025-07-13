-module(start_machines).
-export([start/0]).

%% ממתין עד ששם גלובלי (למשל state_controller) יופיע ברשימת ה-global
wait_until_global(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            timer:sleep(200),
            wait_until_global(Name);
        Pid ->
            io:format("[machines] Found global ~p at ~p~n", [Name, Pid]),
            ok
    end.

start() ->
    net_kernel:connect_node('tables_node@127.0.0.1'),
    net_kernel:connect_node('customers_node@127.0.0.1'),
    net_kernel:connect_node('waiters_node@127.0.0.1'),
    net_kernel:connect_node('safe_node@127.0.0.1'),
     %% המתנה עד ש-state_controller מוכן
    wait_until_global(state_controller),
    application:start(machines).



