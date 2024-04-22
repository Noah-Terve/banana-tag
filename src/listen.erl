-module(listen).

-export([start/1, connect/2, start_display/1]).


connect(Server, PlayerName) ->
    {gameserver, Server} ! {connect, listen, self(), PlayerName}.


start_display(SpawnString) ->
    open_port({spawn, SpawnString}, [binary, {packet, 4}, use_stdio]).


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

%  connect to server
%  start display
%  loop to receive info from server
%  stop when recived stop signal
start([Server, PlayerName, SpawnString]) ->
    Server_name = list_to_atom(Server),
    connect(Server_name, PlayerName),
    Port = start_display(SpawnString),
    Port ! {self(), {command, term_to_binary(PlayerName)}},
    listen_loop(Port).