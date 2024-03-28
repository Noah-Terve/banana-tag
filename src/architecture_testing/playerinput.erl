-module(playerinput).

-export([start/1]).

start(SpawnString) ->
    Port = open_port({spawn, SpawnString}, [binary, {packet, 4}, use_stdio]),
    loop(Port).

loop(Port) ->
    receive 
        {Port, {data, Data}} ->
            Msg = binary_to_term(Data),
            io:format("~s~n", [Msg]),
            {gameserver, 'server@DESKTOP-QTI2JJ6'} ! {msg, self(), Msg}
    end.
