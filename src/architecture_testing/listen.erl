-module(listen).

-export([connect/1]).

-define(SERVER, {gameserver, 'server@vm-hw05'}).


connect(PlayerName) ->
    {gameserver, 'server@vm-hw05'} ! {connect, listen, self(), PlayerName}.
