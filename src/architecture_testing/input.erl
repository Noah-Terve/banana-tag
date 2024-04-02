-module(input).

-export([connect/1  ]).

-define(SERVER, {gameserver, 'server@DESKTOP-QTI2JJ6'}).

connect(PlayerName) ->
    {gameserver, 'server@DESKTOP-QTI2JJ6'} ! {connect, input, self(), PlayerName}.
