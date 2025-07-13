-module(socket_server).
-behaviour(gen_server).

-export([start_link/0, send_to_gui/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([socket_handler/1]). % פונקציית היצוא החדשה

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

send_to_gui(Msg) ->
    io:format("[DEBUG] Sending to GUI: ~p~n", [Msg]),
    gen_server:cast({global, ?MODULE}, {gui_update, Msg}).

init([]) ->
    try_listen(8080).

try_listen(Port) ->
    case gen_tcp:listen(Port, [{active, false}, {packet, 4}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            io:format("[socket_server] listening on port ~p...~n", [Port]),
            spawn_link(fun() -> accept_loop(ListenSocket) end),
            {ok, #{socket => undefined, listen_socket => ListenSocket}};
        {error, eaddrinuse} ->
            io:format("[socket_server] Port ~p is in use. Retrying in 1 second...~n", [Port]),
            timer:sleep(1000),
            try_listen(Port);
        {error, Reason} ->
            {error, Reason}
    end.

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("[socket_server] GUI connected.~n"),
    gen_server:cast({global, ?MODULE}, {new_connection, Socket}),
    accept_loop(ListenSocket).


handle_info(Info, State) ->
    io:format("[socket_server] general handle_info - received: ~p~n", [Info]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({new_connection, Socket}, State) ->
    io:format("[socket_server] New connection accepted. Storing socket: ~p~n", [Socket]),
    % יוצר תהליך ייעודי שיטפל בקבלת הודעות מהסוקט הזה
    spawn_link(fun() -> socket_handler(Socket) end),
    {noreply, State#{socket => Socket}};

handle_cast({send_to_gui, {add_entity, Type, Id, {X,Y}, GuiState}}, ServerState = #{socket := Socket}) ->
    io:format("[socket_server] recived add new ~p to gui ~n", [Type]),
    Msg = lists:flatten(io_lib:format(
        "gui:add_entity:~p:~p:~p:~p:~s", [Type, Id, X, Y, atom_to_list(GuiState)]
    )),
    gen_tcp:send(Socket, list_to_binary(Msg)),
    io:format("[socket_server] sent : ~s ~n", [Msg]),
    {noreply, ServerState};

handle_cast({gui_update, update_state, Type, Id, State, {X, Y}}, ServerState = #{socket := Socket}) ->
    io:format("[socket_server] sending update_state for ~p:~p -> ~p~n", [Type, Id, State]),
    
    Msg = lists:flatten(io_lib:format(
        "gui:update_state:~p:~p:~p:~p:~p", [Type, Id, State, X, Y]
    )),
    
    gen_tcp:send(Socket, list_to_binary(Msg)),
    io:format("[socket_server] sent: ~s~n", [Msg]),
    
    {noreply, ServerState};



handle_cast({tcp_data, Data}, State) ->
    % הודעה שמגיעה מהתהליך החדש, עם נתוני ה-TCP
    io:format("[socket_server] Received data from handler process.~n", []),
    %Message = binary_to_list(Data),
    Message = Data,
    io:format("[socket_server] parsed msg: ~p~n", [Message]),
    Tokens = string:tokens(Message, ":"),
    io:format("[socket_server] sent: ~s~n", [Tokens]),
    handle_tcp_message(maps:get(socket, State), State, Tokens);

handle_cast({gui_update, update_balance, NewBalance}, ServerState = #{socket := Socket}) ->
    Msg = lists:flatten(io_lib:format("gui:update_balance:~p", [NewBalance])),
    gen_tcp:send(Socket, list_to_binary(Msg)),
    io:format("[socket_server] sent: ~s~n", [Msg]),
    {noreply, ServerState};


handle_cast(_Request, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_Old, State, _Extra) -> {ok, State}.

% הפונקציה החדשה שמקבלת הודעות באופן יזום
socket_handler(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_server:cast({global, socket_server}, {tcp_data, Data}),
            socket_handler(Socket);
        {error, closed} ->
            io:format("[socket_server] Socket handler: Connection closed.~n"),
            exit(normal);
        {error, Reason} ->
            io:format("[socket_server] Socket handler: Error: ~p~n", [Reason]),
            exit(Reason)
    end.


% פונקציית עזר למנוע כפל קוד (שונתה כדי לקבל Data)
handle_tcp_message(Socket, State, Tokens) ->
    case Tokens of
        [Target, "get_level", Id] ->
            handle_get_level(Socket, State, list_to_atom(Target), list_to_atom(Id));

        [Target, Command] ->
            handle_simple_command(Socket, State, list_to_atom(Target), list_to_atom(Command));

        [_Target, "upgrade", Type, Id] ->
            handle_upgrade(Socket, State, list_to_atom(Type), list_to_atom(Id));

        [Target, "clean_dirty_table", Id] ->
            handle_clean(Socket, State, list_to_atom(Target), list_to_atom(Id));

        [Target, "get_price", Type] ->
            handle_get_price(Socket, State, list_to_atom(Target), list_to_atom(Type), add);

        [Target, "get_price", Purpose, Type] when Purpose == "add"; Purpose == "upgrade" ->
            handle_get_price(
                Socket,
                State,
                list_to_atom(Target),
                list_to_atom(Type),
                list_to_atom(Purpose)
            );
        [Target, "add", EntityType] ->
            handle_add_entity(Socket, State, list_to_atom(Target), list_to_atom(EntityType));


        _ ->
            io:format("[socket_server] unknown command: ~p~n", [Tokens]),
            {noreply, State}
    end.


% פונקציות עזר לטיפול בבקשות מה-GUI
handle_get_level(Socket, State, TargetAtom, IdAtom) ->
    {ok, Level} = apply(TargetAtom, get_level, [IdAtom]),
    gen_tcp:send(Socket, list_to_binary(io_lib:format("~p", [Level]))),
    {noreply, State}.
handle_simple_command(_Socket, State, TargetAtom, CommandAtom) ->
    CorrectedCommand = case CommandAtom of
        start_game -> rquest_start_game;
        _ -> CommandAtom
    end,
    _Response = gen_server:call({global, TargetAtom}, CorrectedCommand),
    {noreply, State}.


handle_upgrade(Socket, State, TypeAtom, IdAtom) ->
    FuncToCall = list_to_atom("upgrade_" ++ atom_to_list(TypeAtom)),
    Response = gen_server:call({global, player}, {FuncToCall, IdAtom}),
    io:format("[socket_server] upgrade player response  ~p ~n", [Response]),
    ReplyString = case Response of
        ok -> "gui:upgrade_approved";
        {error,not_enough_money} -> "gui:not_enough_money";
        _ -> "gui:unknown_error"
    end,
    io:format("[socket_server] upgrade_response  ~p ~n", [ReplyString]),
    gen_tcp:send(Socket, list_to_binary(ReplyString)),
    {noreply, State}.


handle_clean(Socket, State, TargetAtom, IdAtom) ->
    Response = gen_server:call({global, TargetAtom}, {clean_dirty_table, IdAtom}),
    ReplyString = case Response of
        ok -> "ok";
        {error, not_dirty} -> "not_dirty";
        {error, _Reason} -> "error"
    end,
    gen_tcp:send(Socket, list_to_binary(ReplyString)),
    {noreply, State}.

handle_get_price(Socket, State, TargetAtom, TypeAtom, Purpose) ->
    FullType = list_to_atom(atom_to_list(Purpose) ++ "_" ++ atom_to_list(TypeAtom)),
    Price = gen_server:call({global, TargetAtom}, {get_price, FullType}),
    Msg = case Purpose of
        add -> io_lib:format("gui:show_add_button:~s:~p", [atom_to_list(TypeAtom), Price]);
        upgrade -> io_lib:format("gui:show_upgrade_button:~s:~p", [atom_to_list(TypeAtom), Price])
    end,
    io:format("Sending price ~p for ~p ~p (~p)~n", [Price, Purpose, TypeAtom, FullType]),
    gen_tcp:send(Socket, list_to_binary(Msg)),
    {noreply, State}.

handle_add_entity(Socket, State, TargetAtom, EntityType) ->
    FuncToCall = list_to_atom("add_" ++ atom_to_list(EntityType)),
    Response = gen_server:call({global, TargetAtom}, FuncToCall),
    io:format("[socket_server] response from player  ~p ~n", [Response]),
    ReplyString = case Response of
        ok -> "add_approved";
        {error, not_enough_money} -> "gui:not_enough_money";
        {error, already_exists} -> "gui:already_exists";
        _ -> "gui:unknown_error"
    end,
    gen_tcp:send(Socket, list_to_binary(ReplyString)),
    {noreply, State}.


