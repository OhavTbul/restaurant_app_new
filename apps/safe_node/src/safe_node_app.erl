-module(safe_node_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case task_registry:start_link() of
        {ok, Pid1} -> io:format("[safe_node_app] task_registry started with pid: ~p~n", [Pid1]);
        Error1 -> io:format("[safe_node_app] Failed to start task_registry: ~p~n", [Error1])
    end,
    case order_registry:start_link() of
        {ok, Pid2} -> io:format("[safe_node_app] order_registry started with pid: ~p~n", [Pid2]);
        Error2 -> io:format("[safe_node_app] Failed to start order_registry: ~p~n", [Error2])
    end,
    {ok, self()}.


stop(_State) ->
    ok.
