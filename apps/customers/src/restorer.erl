-module(restorer).
-export([restore_node/1, stop_application/1]).

%% @doc
%% This is the main entry point for restoring.
%% Its job is to start the ENTIRE APPLICATION for the given entity type.
%%
restore_node(EntityType) ->
    % 1. Translate entity type to appropriate application name
    %    (e.g., customers -> customers)
    %    In our case, the names are identical, so it's simple.
    AppName = EntityType,
    
    io:format("[Restorer on ~p] Received command. Attempting to start application '~p'...~n", [node(), AppName]),
    
    % 2. Start the entire application.
    %    This is the correct command. It will take care of starting the supervisor and everything needed.
    case application:ensure_all_started(AppName) of
        {ok, _StartedApps} ->
            io:format("[Restorer] Successfully started application '~p'.~n", [AppName]);

        {error, {AppName, {already_started, AppName}}} ->
            % This happens if we try to restore an entity that is already running on the node.
            % For example, if the waiters_node receives an order to restore waiters.
            % This is completely normal.
            io:format("[Restorer] Application '~p' was already running. No action taken.~n", [AppName]);

        {error, Reason} ->
            io:format("[Restorer] FAILED to start application '~p'. Reason: ~p~n", [AppName, Reason])
    end.

stop_application(EntityType) ->
    AppName = EntityType, % In our case, the names are the same
    io:format("[Restorer on ~p] Received command to STOP application '~p'...~n", [node(), AppName]),
    case application:stop(AppName) of
        ok ->
            io:format("[Restorer] Successfully stopped application '~p'.~n", [AppName]);
        {error, {not_started, AppName}} ->
            io:format("[Restorer] Application '~p' was not running. Nothing to stop.~n", [AppName]);
        {error, Reason} ->
            io:format("[Restorer] FAILED to stop application '~p'. Reason: ~p~n", [AppName, Reason])
    end.