-module(listen).

-export([start/1, connect/1, start_display/1]).

-define(SERVER, {gameserver, 'server@vm-hw05'}).


connect(PlayerName) ->
    {gameserver, 'server@vm-hw05'} ! {connect, listen, self(), PlayerName}.


start_display(SpawnString) ->
    open_port({spawn, SpawnString}, [binary, {packet, 4}, use_stdio]).


listen_loop(Port) ->
    receive 
        {update, GameState} ->
            % case GameState of 
            %     [{I1, I2, I3, S1}] -> io:format("Received msg from server ~B ~B ~B ~s~n", [I1, I2, I3, S1]);
            %     _ -> io:format("Error", [])
            % end,
            Port ! {self(), {command, term_to_binary(GameState)}},

            listen_loop(Port);
        stop -> Port ! {self(), close};
        MSG -> io:format(MSG)
    end.

%  connect to server
%  start display
%  loop to receive info from server
%  stop when recived stop signal
start([PlayerName, SpawnString]) ->
    connect(PlayerName),
    Port = start_display(SpawnString),
    Port ! {self(), {command, term_to_binary(PlayerName)}},
    listen_loop(Port).