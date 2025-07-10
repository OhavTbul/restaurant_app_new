
-module(cashier).
-behaviour(gen_server).

-export([start_link/0, insert_money/1, spend_money/1, get_balance/0, get_transaction_log/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    balance = 0
}).

%%% ================================
%%% API
%%% ================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert_money(Amount) when is_integer(Amount), Amount > 0 ->
    gen_server:call(?MODULE, {insert_money, Amount}).

spend_money(Amount) when is_integer(Amount), Amount > 0 ->
    gen_server:call(?MODULE, {spend_money, Amount}).

get_balance() ->
    gen_server:call(?MODULE, get_balance).

get_transaction_log() ->
    ets:tab2list(cashier_log).

%%% ================================
%%% gen_server callbacks
%%% ================================

init([]) ->
    ets:new(cashier_log, [named_table, public, bag]),
    {ok, #state{balance = 0}}.

handle_call({insert_money, Amount}, _From, State = #state{balance = Balance}) ->
    NewBalance = Balance + Amount,
    log_transaction(insert, Amount, NewBalance),
    {reply, {ok, NewBalance}, State#state{balance = NewBalance}};

handle_call({spend_money, Amount}, _From, State = #state{balance = Balance}) ->
    case Balance >= Amount of
        true ->
            NewBalance = Balance - Amount,
            log_transaction(spend, Amount, NewBalance),
            {reply, ok, State#state{balance = NewBalance}};
        false ->
            {reply, {error, not_enough_money}, State}
    end;

handle_call(get_balance, _From, State = #state{balance = Balance}) ->
    {reply, Balance, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%% ================================
%%% Helper
%%% ================================

log_transaction(Type, Amount, NewBalance) ->
    Timestamp = calendar:universal_time(),
    ets:insert(cashier_log, {Timestamp, Type, Amount, NewBalance}).
