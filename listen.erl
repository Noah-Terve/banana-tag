% listen.erl
% Erlang node that listens for communication from the server and then forwards
% it to the python display on the client side.

-module(listen).

-export([start/1, connect/2, start_display/1]).

% connect
% Given a server name, and a player name, tell the server this node is up.
connect(Server, PlayerName) ->
    {gameserver, Server} ! {connect, listen, self(), PlayerName}.

% start_display
% Given an executable string, spawn a process with a port to it.
start_display(SpawnString) ->
    open_port({spawn, SpawnString}, [binary, {packet, 4}, use_stdio]).

% listen_loop
% Given a port, listen for updates from the gameserver and forward them to the
% python at the end of the port.
listen_loop(Port) ->
    receive 
        {update, GameState} ->
            Port ! {self(), {command, term_to_binary(GameState)}},
            listen_loop(Port);
        stop -> Port ! {self(), {command, term_to_binary(close)}};
        {Port, {data, Msg}} ->
            case binary_to_term(Msg) of
                close -> ok
            end;
        MSG -> io:format(MSG)
    end.

% start
% Given a server, a player name, and an executable string, connect to server
% start the display, and loop to receive info from server
start([Server, PlayerName, SpawnString]) ->
    Server_name = list_to_atom(Server),
    connect(Server_name, PlayerName),
    Port = start_display(SpawnString),
    Port ! {self(), {command, term_to_binary(PlayerName)}},
    listen_loop(Port).