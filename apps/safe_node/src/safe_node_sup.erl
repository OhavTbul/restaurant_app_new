-module(safe_node_sup).
-behaviour(supervisor).
-behaviour(application).

-export([start_link/1, init/1]).
-export([start/2, stop/1]).

%%%------------------------------------------------------
%%% הפעלה של הסופרווייזור הראשי של safe_node
%%%------------------------------------------------------

start_link(Nodes) ->
    % חשוב לרשום אותו בשם local כדי שה-state_controller יוכל לגשת אליו
    supervisor:start_link({local, safe_node_sup}, ?MODULE, Nodes).

%%%------------------------------------------------------
%%% אתחול התהליכים הקבועים של SAFE NODE
%%%------------------------------------------------------

init(Nodes) ->
    io:format("[safe_node_sup] Initializing SAFE NODE supervisor...~n"),

    % תהליכים קבועים של SAFE NODE
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

    % הגדרת מדיניות השגחה - one_for_one מאפשר לנהל ילדים נוספים דינמית
    {ok, {{one_for_one, 10, 10}, Children}}.

%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(_StartType, _StartArgs) ->
    % 1. קרא את רשימת ה-nodes מהסביבה שהגדרת בקובץ ה-.app
    case application:get_env(safe_node, nodes_to_manage) of
        {ok, NodesToManage} ->
             % 2. קרא לפונקציית ה-start_link שלך עם הרשימה הזו
            ?MODULE:start_link(NodesToManage);
        undefined ->
            io:format("ERROR: 'nodes_to_manage' not defined in .app file!~n"),
            {error, nodes_not_defined}
    end.


stop(_State) ->
    ok.
