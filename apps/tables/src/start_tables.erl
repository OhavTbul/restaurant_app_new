-module(start_tables).
-export([start/0]).

%% פונקציה שמחכה עד ששם גלובלי יופיע ב־global registry
wait_until_global(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            timer:sleep(200),
            wait_until_global(Name);
        Pid ->
            io:format("[tables] Found global ~p at ~p~n", [Name, Pid]),
            ok
    end.

start() ->
    net_kernel:connect_node('customers_node@127.0.0.1'),
    net_kernel:connect_node('machines_node@127.0.0.1'),
    net_kernel:connect_node('waiters_node@127.0.0.1'),
    net_kernel:connect_node('safe_node@127.0.0.1'),
        %% חכה ש-state_controller יהיה גלוי
    wait_until_global(state_controller),
    application:start(tables).

