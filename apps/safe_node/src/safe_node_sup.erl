-module(safe_node_sup).
-behaviour(supervisor).
-behaviour(application).

-export([start_link/1, init/1]).
-export([start/2, stop/1]).

%%%------------------------------------------------------
%%% Main supervisor startup for safe_node
%%%------------------------------------------------------

start_link(Nodes) ->
    % Register locally so state_controller can access it
    supervisor:start_link({local, safe_node_sup}, ?MODULE, Nodes).

%%%------------------------------------------------------
%%% Initialize permanent SAFE NODE processes
%%%------------------------------------------------------

init(Nodes) ->
    io:format("[safe_node_sup] Initializing SAFE NODE supervisor...~n"),

    % Permanent SAFE NODE processes
    Children = [
        {state_controller, {state_controller, start_link, [Nodes]},
         permanent, 5000, worker, [state_controller]},

        {task_registry, {task_registry, start_link, []},
         permanent, 5000, worker, [task_registry]},

        {order_registry, {order_registry, start_link, []},
         permanent, 5000, worker, [order_registry]},
        
        {player, {player, start_link, []},
         permanent, 5000, worker, [player]},
        
        {cashier, {cashier, start_link, []},
         permanent, 5000, worker, [cashier]},

        {socket_server, {socket_server, start_link, []},
         permanent, 5000, worker, [socket_server]}

         
    ],

    % Supervision policy - one_for_one allows dynamic child management
    {ok, {{one_for_one, 10, 10}, Children}}.

%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(_StartType, _StartArgs) ->
    % 1. Read nodes list from environment defined in .app file
    case application:get_env(safe_node, nodes_to_manage) of
        {ok, NodesToManage} ->
             % 2. Call start_link with this list
            ?MODULE:start_link(NodesToManage);
        undefined ->
            io:format("ERROR: 'nodes_to_manage' not defined in .app file!~n"),
            {error, nodes_not_defined}
    end.


stop(_State) ->
    ok.
