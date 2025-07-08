-module(customer_sup).
-behaviour(supervisor).
-behaviour(application).

-export([start_link/0, init/1, start_client/1, update_customer_state/2]).
-export([handle_info/2]).

-define(TABLE_STATE, customer_state).
-define(ETS_UPDATE_TIME, 5000). % 5sec between ETS updates


-export([start/2, stop/1]).
%%%===================
%%% API
%%%===================

start_link() -> %start sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case ets:info(?TABLE_STATE) of
        undefined ->
            ets:new(?TABLE_STATE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[customer_sup] ETS table ~p created.~n", [?TABLE_STATE]);
        _ ->
            ok % Table already exists, do nothing
    end,
    %maneger procces - communicate with safe node
    MngSpec = {
        customer_mng,
        {customer_mng, start_link, []},
        transient, 
        5000,
        worker,
        [customer_mng]
    },
    %%Define child spec - customer_fsm - simple child that can be duplicate
    {ok, {{one_for_one, 5, 10}, [MngSpec]}}.

%%%===================
%%% Start & Restart Customers
%%%===================

start_client(CustomerId) ->
    %% new fsm
     ChildId = list_to_atom("customer_fsm_" ++ integer_to_list(CustomerId)),

    ChildSpec = {
        ChildId,                                    
        {customer_fsm, start_link, [CustomerId]},     
        transient,                                  
        5000,                                      
        worker,                                     
        [customer_fsm]                               
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            ets:insert(?TABLE_STATE, {CustomerId, #{pid => Pid}}),
            io:format("customer ~p started.~n", [CustomerId]),
            ok;
        {error, {already_present, _}} ->
            io:format("customer ~p already exists.~n", [CustomerId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start customer ~p: ~p~n", [CustomerId, Reason]),
            {error, Reason}
    end.



%%%===================
%%% Customer sends updated state periodically
%%%===================

-spec update_customer_state(CustomerId :: term(), Data :: map()) -> ok.
update_customer_state(CustomerId, Data) ->
    %% Data must include pid, pos, state, table, etc.
    ets:insert(?TABLE_STATE, {CustomerId, Data}),
    ok.

%%%===================
%%% Handle customer crash and restart
%%%===================


handle_info(_, State) ->
    {noreply, State}.


%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(_StartType, _StartArgs) ->
    ?MODULE:start_link().


stop(_State) ->
    ok.
