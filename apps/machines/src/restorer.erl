-module(restorer).
-export([restore_node/1]).

%% @doc
%% This is the main entry point for restoring.
%% Its job is to start the ENTIRE APPLICATION for the given entity type.
%%
restore_node(EntityType) ->
    % 1. תרגם את סוג הישות לשם האפליקציה המתאימה
    %    (למשל, customers -> customers)
    %    במקרה שלנו, השמות זהים, אז זה פשוט.
    AppName = EntityType,
    
    io:format("[Restorer on ~p] Received command. Attempting to start application '~p'...~n", [node(), AppName]),
    
    % 2. הפעל את האפליקציה כולה.
    %    זו הפקודה הנכונה. היא תדאג להפעיל את ה-supervisor וכל מה שצריך.
    case application:ensure_all_started(AppName) of
        {ok, _StartedApps} ->
            io:format("[Restorer] Successfully started application '~p'.~n", [AppName]);

        {error, {AppName, {already_started, AppName}}} ->
            % זה קורה אם אנחנו מנסים לשחזר ישות שכבר רצה על ה-node.
            % למשל, אם ה-waiters_node מקבל הוראה לשחזר את waiters.
            % זה תקין לחלוטין.
            io:format("[Restorer] Application '~p' was already running. No action taken.~n", [AppName]);

        {error, Reason} ->
            io:format("[Restorer] FAILED to start application '~p'. Reason: ~p~n", [AppName, Reason])
    end.