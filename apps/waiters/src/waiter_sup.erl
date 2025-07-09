-module(waiter_sup).
-behaviour(supervisor).
-behaviour(application).

-export([
    start_link/0,
    init/1,
    start_waiter/1,
    upgrade_waiter/1,
    update_waiter_state/2,
    handle_info/2
]).

-define(TABLE, waiter_state).
-define(REPORT_INTERVAL, 5000). %5sec
-export([start/2, stop/1]).

%%%===================
%%% API
%%%===================

start_link() -> %create sup
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
            io:format("[waiter_sup] ETS table ~p created.~n", [?TABLE]);
        _ ->
            ok % Table already exists, do nothing
    end,
    %maneger procces - communicate with safe node
    MngSpec = {
        waiter_mng,
        {waiter_mng, start_link, []},
        transient, 
        5000,
        worker,
        [waiter_mng]
    },
    %%Define child spec - customer_fsm - simple child that can be duplicate

    {ok, {{one_for_one, 5, 10}, [MngSpec]}}.

%%%===================
%%% create new machine
%%%===================

start_waiter(WaiterId) ->
    %% new fsm
    ChildId = list_to_atom("waiter_fsm_" ++ atom_to_list(WaiterId)),

    ChildSpec = {
        ChildId,                                    
        {waiter_fsm, start_link, [WaiterId]},     
        transient,                                  
        5000,                                      
        worker,                                     
        [waiter_fsm]                               
    },

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            ets:insert(?TABLE, {WaiterId, #{pid => Pid, upgrade_level => 0}}),
            io:format("waiter ~p started.~n", [WaiterId]),
            ok;
        {error, {already_present, _}} ->
            io:format("waiter ~p already exists.~n", [WaiterId]),
            ok;
        {error, Reason} ->
            io:format("Failed to start waiter ~p: ~p~n", [WaiterId, Reason]),
            {error, Reason}
    end.

%%%===================
%%% upgrade machine
%%%===================

upgrade_waiter(WaiterId) ->
    Name = list_to_atom("waiter_fsm_" ++ atom_to_list(WaiterId)),
    case whereis(Name) of
        undefined ->
            io:format("Cannot upgrade. waiter ~p not found.~n", [WaiterId]),
            {error, not_found};
        _ ->
            waiter_fsm:upgrade(WaiterId)
            ok
    end.

%%%===================
%%% update waiter
%%%===================

update_waiter_state(WaiterId, Data) ->
    ets:insert(?TABLE, {WaiterId, Data}),
    ok.


%%%===================
%%% Handle info
%%%===================
%%%
%for unexpected msgs
handle_info(_, State) ->
    {noreply, State}.


%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(_StartType, _StartArgs) ->
    ?MODULE:start_link().


stop(_State) ->
    ok.