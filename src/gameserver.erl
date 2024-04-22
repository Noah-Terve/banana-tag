-module(gameserver).

-export([start/1, reg_server/1, loop/2]).

start(ServerFile) ->
    register(gameserver, spawn(gameserver, reg_server, [ServerFile])).

reg_server(ServerFile) ->
    Port = open_port({spawn, ServerFile}, [binary, {packet, 4}, use_stdio]),
    io:format("port is open", []),
    receive
        {Port, {data, Bin}} ->
            case binary_to_term(Bin) of
                go -> io:format("----- running simulation!~n", []);
                T  -> io:format("!!! unexpected: ~w~n", [T])
            end
    end,
    loop(Port, []).


% Update all listeners in the game
updateListeners([], _GameState) ->
    ok;
updateListeners([Listener | Listeners], GameState) ->
    Listener ! {update, binary_to_term(GameState)},
    updateListeners(Listeners, GameState).

updateListeners2([], _Msg) ->
    ok;
updateListeners2([Listener | Listeners], Msg) ->
    Listener ! {update, Msg},
    updateListeners(Listeners, Msg).

% {name, {inputPid, listenPid}}
loop(Port, ListenPids) ->
    receive 
        {connect, input, _Pid, PlayerName} ->
            io:format("Received connection from input node of Player ~s~n", [PlayerName]),
            loop(Port, ListenPids);

        {connect, listen, Pid, PlayerName} ->
            io:format("Received connection from listen node of Player ~s~p~n", [PlayerName, Pid]),
            Port ! {self(), {command, term_to_binary(PlayerName)}},
            loop(Port, [Pid | ListenPids]);

        {keystroke, Key, _Pid, PlayerName} ->
            io:format("Received ~c from Player ~s~n", [Key, PlayerName]),
            Port ! {self(), {command, term_to_binary({PlayerName, Key})}},
            loop(Port, ListenPids);
        
        {Port, {data, Bin}} ->
            case binary_to_term(Bin) of
                {update, GameState} -> 
                    updateListeners(ListenPids, GameState);
                T  -> io:format("!!! unexpected server.py to gameserver.erl: 
                                ~w~n", [T])
            end,
            loop(Port, ListenPids);

        stop -> 
            updateListeners2(ListenPids, stop),
            Port ! {self(), {command, term_to_binary(close)}},
            ok;
        
        restart -> 
            updateListeners2(ListenPids, stop),
            Port ! {self(), {command, term_to_binary(restart)}},
            loop(Port, ListenPids)
    end.
