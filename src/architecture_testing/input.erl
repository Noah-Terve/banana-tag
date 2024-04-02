-module(input).

-export([connect/1]).

-define(SERVER, {gameserver, 'server@vm-hw05'}).

connect(PlayerName) ->
    % ?SERVER ! {connect, input, self(), PlayerName}.
    {gameserver, 'server@vm-hw05'} ! {connect, input, self(), PlayerName}.
