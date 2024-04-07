-module(port_manager).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([send/1, dump_messages/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link(SpawnString) ->
    gen_server:start_link({local, port_manager}, port_manager, [SpawnString], []).

stop() ->
    gen_server:stop(port_manager).

send(Data) -> gen_server:cast(port_manager, {send, Data}).
dump_messages() -> gen_server:call(port_manager, dump_messages).

init(SpawnString) ->
    Port = open_port({spawn, SpawnString}, [binary, {packet,4}, use_stdio]),
    {ok, {Port, []}}.

%% When we terminate, send the program on the port a message indicating that
%% it is about to close, then close the port.
terminate(_Reason, {Port, _Messages}) ->
    Port ! {self(), command, term_to_binary(close)},
    Port ! {self(), close}.

handle_cast({send, Data}, {Port, Messages}) ->
    Port ! {self(), {command, term_to_binary(Data)}},
    {noreply, {Port, Messages}}.

handle_call(dump_messages, _From, {Port, Messages}) -> 
    {reply, Messages, {Port, []}}.


%% If we recieve a message from the port, store it in our messages list
handle_info({Port, {data, Data}}, {Port, Messages}) ->
    {noreply, {Port, [binary_to_term(Data) | Messages]}}.