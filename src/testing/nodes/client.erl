-module(client).

-export([start/0, loop/0]).

start() ->
    loop().

loop() ->
    case io:get_line("Enter your input: ") of
        "quit\n" ->
            io:format("Exiting...~n");
        Input ->
            io:format("You entered: ~s~n", [Input]),
            {gameserver, 'server@vm-hw06.eecs.tufts.edu'} ! {msg, self(), Input},
            loop()
    end.