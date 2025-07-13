-module(safe_node_sup).
-behaviour(supervisor).
-behaviour(application).

-export([start_link/0, init/1]).
-export([start/2, stop/1]).

%%%------------------------------------------------------
%%% הפעלה של הסופרווייזור הראשי של safe_node
%%%------------------------------------------------------

start_link() ->
    % חשוב לרשום אותו בשם local כדי שה-state_controller יוכל לגשת אליו
    supervisor:start_link({local, safe_node_sup}, ?MODULE, []).

%%%------------------------------------------------------
%%% אתחול התהליכים הקבועים של SAFE NODE
%%%------------------------------------------------------

init([]) ->
    io:format("[safe_node_sup] Initializing SAFE NODE supervisor...~n"),

    % תהליכים קבועים של SAFE NODE
    Children = [
        {state_controller, {state_controller, start_link, []},
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
    ?MODULE:start_link().


stop(_State) ->
    ok.
