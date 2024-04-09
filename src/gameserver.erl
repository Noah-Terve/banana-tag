-module(gameserver).

-export([start/1, reg_server/1, loop/1]).

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
    loop(Port).

loop(Port) ->
    io:format("Iteration~n", []),
    receive 
        {connect, input, _Pid, PlayerName} ->
            io:format("Received connection from input node of Player ~s~n", [PlayerName]),
            loop(Port);
        {connect, listen, _Pid, PlayerName} ->
            io:format("Received connection from listen node of Player ~s~n", [PlayerName]),
            loop(Port);
        {keystroke, Key, _Pid, PlayerName} ->
            io:format("Received ~c from Player ~s~n", [Key, PlayerName]),
            Port ! {self(), {command, term_to_binary({PlayerName, Key})}},
            loop(Port);
        {stop} -> 
            Port ! {self(), close},
            ok
    end.

