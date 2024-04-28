% input.erl
% The erlang node that collects player input and sends it to the server.

-module(input).

-export([start/1, connect/2]).

% connect
% given a server name, and a playername tell the server that this player
% is connected
connect(Server, PlayerName) ->
    {gameserver, Server} ! {connect, input, self(), PlayerName}.

% get_char
% loop getting characters from input and sending that input to the gameserver
get_char(Server, PlayerName) ->
    case binary_to_list(io:get_chars("Enter a character: ", 1)) of
        [Char] when Char == $q -> 
            io:format("TERMINATING");
        [Char] when Char == $a; Char == $w; Char == $s; Char == $d; Char == $r;
                    Char == $p; Char == $z ->
            {gameserver, Server} ! {keystroke, Char, self(), PlayerName},
            get_char(Server, PlayerName);
        [] -> ok;
        _ -> get_char(Server, PlayerName)
    end.

% start
% Given a server and a player name, start the player input collection by telling
% the server this is set up, and listening for input from the player.
start([Server, PlayerName]) ->
    Server_name = list_to_atom(Server),
    connect(Server_name, PlayerName),
    io:setopts([{binary, true}]),
    get_char(Server_name, PlayerName).