-module(customer_mng).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([send_state_to_safe/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-define(TABLE, customer_state).
-define(REPORT_INTERVAL, 2000). 

%%%===================================================================
%%% API
%%%===================================================================

start_link(RestoredCustomers) -> % Changed to arity 1
    gen_server:start_link({global, ?MODULE}, ?MODULE, RestoredCustomers, []).

send_state_to_safe() ->
    gen_server:cast({global, ?MODULE}, send_report).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(RestoredCustomers) -> 
    io:format("[customer_mng] init/1 called~n"),
    NextId = if %calculate next ID
        is_list(RestoredCustomers), RestoredCustomers =/= [] ->
            MaxId = lists:max([CustomerId || {CustomerId, _} <- RestoredCustomers]),
            io:format("[customer_mng] Restored state. Next customer ID will be ~p~n", [MaxId + 1]),
            MaxId + 1;
        true ->
            io:format("[customer_mng] Starting fresh. Next customer ID will be 1~n"),
            1
    end,

    erlang:send_after(?REPORT_INTERVAL, self(), report_to_safe),
    erlang:send_after(1000, self(), generate_customer),
    {ok, #{next_customer_id => NextId}}.

handle_info(report_to_safe, State) ->
    %% Trigger send
    send_state_to_safe(),
    %% Schedule next report
    erlang:send_after(?REPORT_INTERVAL, self(), report_to_safe),
    {noreply, State};

%% New clause for customer creation
handle_info(generate_customer, State) ->
    CurrentCustomerId = maps:get(next_customer_id, State, 1), % Take the current ID
    CustomerId = CurrentCustomerId, % Use it as ID for the new customer
    NewState = State#{next_customer_id => CurrentCustomerId + 1}, % Increment counter and save in new state
    
    %% Call the supervisor's API to start a new customer
    customer_sup:start_client(CustomerId),
    io:format("[customer_mng] Generated new customer with ID: ~p~n", [CustomerId]), % <--- Debug message
    
    %% Calculate next time and schedule the next event
    Lambda = 0.3,
    NextDelayMs = trunc(-math:log(rand:uniform()) / Lambda * 1000),
    erlang:send_after(NextDelayMs, self(), generate_customer),

    {noreply, NewState}; % <--- Return the updated state

handle_info(_, State) ->
    {noreply, State}.

handle_cast(send_report, State) ->
    AllCustomersData = ets:tab2list(?TABLE),
    % Send data to central controller
    gen_server:cast({global, state_controller}, {update, customers, AllCustomersData}),
    io:format("[customer_mng] Sending ~p customer states to SAFE NODE~n", [length(AllCustomersData)]),
    {noreply, State};

handle_cast({take_over_responsibilities, EntityTypes}, State) ->
    lists:foreach(
      fun(AnEntityType) ->
          % --> Here is the spawn! <--
          spawn(fun() -> restorer:restore_node(AnEntityType) end)
      end,
      EntityTypes
    ),
    {noreply, State};

handle_cast({relinquish_responsibility, EntityType}, State) ->
    io:format("[~p] Received order to relinquish responsibility for '~p'. Stopping application...~n", [?MODULE, EntityType]),
    % Call restorer to shut down the appropriate application
    restorer:stop_application(EntityType),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}. % Removed trailing backslash

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}. % Removed trailing backslash

terminate(_Reason, _State) ->
    ok. % Removed trailing backslash

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. % Removed trailing backslash
