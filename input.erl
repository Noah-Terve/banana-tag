-module(input).

-export([start/1, connect/2]).

connect(Server, PlayerName) ->
    % ?SERVER ! {connect, input, self(), PlayerName}.
    {gameserver, Server} ! {connect, input, self(), PlayerName}.


get_char(Server, PlayerName) ->
    case binary_to_list(io:get_chars("Enter a character: ", 1)) of
        [Char] when Char == $q -> 
            io:format("TERMINATING");
        [Char] when Char == $a; Char == $w; Char == $s; Char == $d; Char == $r;
                    Char == $p; Char == $z ->
            {gameserver, Server} ! {keystroke, Char, self(), PlayerName},
            % io:format("Got Char: ~c~n", [Char]),
            get_char(Server, PlayerName);
        [] -> ok;
        _ -> get_char(Server, PlayerName)
    end.

start([Server, PlayerName]) ->
    Server_name = list_to_atom(Server),
    connect(Server_name, PlayerName),
    io:setopts([{binary, true}]),
    get_char(Server_name, PlayerName).