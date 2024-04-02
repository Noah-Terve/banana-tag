-module(listen).

-export([connect/1]).

-define(SERVER, {gameserver, 'server@DESKTOP-QTI2JJ6'}).


connect(PlayerName) ->
    {gameserver, 'server@vm-hw05'} ! {connect, listen, self(), PlayerName}.
