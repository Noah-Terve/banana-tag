-module(server).

-export[start/0, loop/1].


start() ->
    register(gameserver, spawn(server, loop, [[]])).

loop(_CurrentState) ->
    receive
        {msg, Pid, Message} ->
            io:format("Got message from ~p: ~s~n", [Pid, Message]),
            loop(_CurrentState);
        {stop} -> ok
    end.
