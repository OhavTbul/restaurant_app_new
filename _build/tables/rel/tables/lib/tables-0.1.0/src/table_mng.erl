-module(table_mng).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([send_state_to_safe/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-define(TABLE, table_state_ets).
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
    All = ets:tab2list(?TABLE),
    %% todo: Replace this with actual message to SAFE NODE
    io:format("[table_mng] Sending ~p table states to SAFE NODE~n", [length(All)]),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
