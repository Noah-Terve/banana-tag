-module(playerinput).

-export([start/1]).

% erl -sname client -run playerinput start 'python3 playerinput.py'

start(SpawnString) ->
    Port = open_port({spawn, SpawnString}, [binary, {packet, 4}, use_stdio]),
    loop(Port).

loop(Port) ->
    io:format("Waiting for Msg ~n", []),
    receive 
        {Port, {data, Data}} ->
            io:format("In loop ~n", []),
            Msg = binary_to_term(Data),
            io:format("~s~n", [Msg]),
            {gameserver, 'server@DESKTOP-QTI2JJ6'} ! {msg, self(), Msg}
    end.
