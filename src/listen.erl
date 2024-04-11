-module(listen).

-export([connect/1, start_display/1]).

-define(SERVER, {gameserver, 'server@vm-hw05'}).


connect(PlayerName) ->
    {gameserver, 'server@vm-hw05'} ! {connect, listen, self(), PlayerName}.


start_display(SpawnString) ->
    _Port = open_port({spawn, SpawnString}, [binary, {packet, 4}, use_stdio]).
    % call function that listens for messages from game server.erl


%  connect to server
%  start display
%  loop to receive info from server
%  stop when recived stop signal