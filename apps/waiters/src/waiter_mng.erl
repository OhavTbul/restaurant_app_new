-module(waiter_mng).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([send_state_to_safe/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3, get_level/1]).

-define(TABLE, waiter_state).
-define(REPORT_INTERVAL, 10000). % 10 sec

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

send_state_to_safe() ->
    gen_server:cast({global, ?MODULE}, send_report).

get_level(WaiterId) ->
    gen_server:call({global, ?MODULE}, {get_level, WaiterId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start periodic report timer
    erlang:send_after(?REPORT_INTERVAL, self(), report_to_safe),

    {ok, #{}}.

handle_info(report_to_safe, State) ->
    %% Trigger send
    send_state_to_safe(),
    %% Schedule next report
    erlang:send_after(?REPORT_INTERVAL, self(), report_to_safe),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

handle_cast(send_report, State) ->
    AllWaitersData = ets:tab2list(?TABLE),
    % שליחת המידע לבקר המרכזי
    gen_server:cast({global, state_controller}, {update, waiters, AllWaitersData}),
    io:format("[waiter_mng] Sending ~p waiter states to SAFE NODE~n", [length(AllWaitersData)]),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_call({get_level, WaiterId}, _From, State) ->
    case ets:lookup(?TABLE, WaiterId) of
        [{_Id, Map}] ->
            Level = maps:get(speed_level, Map, 0),
            {reply, {ok, Level}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({start_waiter, WaiterId}, _From, State) ->
    Result = waiter_sup:start_waiter(WaiterId),
    {reply, Result, State};

handle_call({upgrade_waiter, WaiterId}, _From, State) ->
    Result = waiter_sup:upgrade_waiter(WaiterId),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
