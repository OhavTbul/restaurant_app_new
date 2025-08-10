-module(general_start).
-export([start/1]).

%% @doc
%% The single entry point for starting any node in the system.
%% Receives an atom representing the node type to start,
%% e.g., 'safe', 'customers', 'waiters', etc.
%%
start(NodeType) ->
    AllNodes = [
        'safe_node@127.0.0.1',
        'tables_node@127.0.0.1',
        'customers_node@127.0.0.1',
        'machines_node@127.0.0.1',
        'waiters_node@127.0.0.1'
    ],

    NodesToConnect = lists:delete(node(), AllNodes),
    io:format("[~p] Connecting to other nodes: ~p~n", [?MODULE, NodesToConnect]),
    lists:foreach(fun(Node) -> net_kernel:connect_node(Node) end, NodesToConnect),

    case NodeType of
        safe ->
            application:start(safe_node);
        
        customers ->
            wait_until_global(state_controller),
            application:start(customers);

        waiters ->
            wait_until_global(state_controller),
            application:start(waiters);

        machines ->
            wait_until_global(state_controller),
            application:start(machines);

        tables ->
            wait_until_global(state_controller),
            application:start(tables)
    end.


wait_until_global(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            timer:sleep(200),
            wait_until_global(Name);
        Pid ->
            io:format("Found global process '~p' at ~p. Continuing startup.~n", [Name, Pid]),
            ok
    end.