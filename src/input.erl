-module(input).

-export([start/1, connect/1]).

-define(SERVER, {gameserver, 'server@vm-hw05'}).

connect(PlayerName) ->
    % ?SERVER ! {connect, input, self(), PlayerName}.
    {gameserver, 'server@vm-hw05'} ! {connect, input, self(), PlayerName}.


get_char(PlayerName) ->
    case binary_to_list(io:get_chars("Enter a character: ", 1)) of
        [Char] when Char == $q -> 
            io:format("TERMINATING");
        [Char] ->
            {gameserver, 'server@vm-hw05'} ! {keystroke, Char, self(), PlayerName},
            io:format("Got Char: ~c~n", [Char]),
            get_char(PlayerName);
        [] -> io:format("No input received~n");
        _ -> io:format("Too many characters entered~n")
    end.

start(PlayerName) ->
    connect(PlayerName),
    io:setopts([{binary, true}]),
    get_char(PlayerName).