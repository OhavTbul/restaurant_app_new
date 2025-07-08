-module(customer_mng).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([send_state_to_safe/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-define(TABLE, customer_state).
-define(REPORT_INTERVAL, 20000). % 20 sec

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_state_to_safe() ->
    gen_server:cast(?MODULE, send_report).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start periodic report timer
    erlang:send_after(?REPORT_INTERVAL, self(), report_to_safe),
    erlang:send_after(1000, self(), generate_customer),
    {ok, #{next_customer_id => 1}}. % <--- אתחל מונה לקוחות חדש

handle_info(report_to_safe, State) ->
    %% Trigger send
    send_state_to_safe(),
    %% Schedule next report
    erlang:send_after(?REPORT_INTERVAL, self(), report_to_safe),
    {noreply, State};

%% Clause חדש עבור יצירת לקוחות
handle_info(generate_customer, State) ->
    CurrentCustomerId = maps:get(next_customer_id, State, 1), % קח את המזהה הנוכחי
    CustomerId = CurrentCustomerId, % השתמש בו כ-ID ללקוח החדש
    NewState = State#{next_customer_id => CurrentCustomerId + 1}, % הגדל את המונה ושמור במצב החדש
    
    %% Call the supervisor's API to start a new customer
    customer_sup:start_client(CustomerId),
    io:format("[customer_mng] Generated new customer with ID: ~p~n", [CustomerId]), % <--- הודעת דיבוג
    
    %% Calculate next time and schedule the next event
    Lambda = 0.3,
    NextDelayMs = trunc(-math:log(rand:uniform()) / Lambda * 1000),
    erlang:send_after(NextDelayMs, self(), generate_customer),

    {noreply, NewState}; % <--- החזר את המצב המעודכן

handle_info(_, State) ->
    {noreply, State}.

handle_cast(send_report, State) ->
    All = ets:tab2list(?TABLE), % Removed trailing backslash
    %% todo: Replace this with actual message to SAFE NODE
    io:format("[customer_mng] Sending ~p customer states to SAFE NODE~n", [length(All)]), % Removed trailing backslash
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}. % Removed trailing backslash

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}. % Removed trailing backslash

terminate(_Reason, _State) ->
    ok. % Removed trailing backslash

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. % Removed trailing backslash
