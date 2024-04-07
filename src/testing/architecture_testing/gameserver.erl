-module(gameserver).

-export([start/1, loop/1]).

start(ServerFile) ->
    _Port = open_port({spawn, ServerFile}, [binary, {packet, 4}, use_stdio]),
    register(gameserver, spawn(gameserver, loop, [maps:new()])).


loop(_NameMap) ->
    io:format("Iteration~n", []),
    receive 
        {connect, input, _Pid, PlayerName} ->
            io:format("Received connection from input node of Player ~s~n", [PlayerName]),
            loop(_NameMap);
        {connect, listen, _Pid, PlayerName} ->
            io:format("Received connection from listen node of Player ~s~n", [PlayerName]),
            loop(_NameMap);
        {keystroke, Key, _Pid, PlayerName} ->
            io:format("Received ~c from Player ~s~n", [Key, PlayerName]),
            loop(_NameMap);
        {stop} -> ok
    end.




