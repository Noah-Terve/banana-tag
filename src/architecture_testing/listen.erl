-module(listen).

-export([connect/1]).

-define(SERVER, {gameserver, 'server@DESKTOP-QTI2JJ6'}).


connect(PlayerName) ->
    ?SERVER ! {connect, listen, self(), PlayerName}.
