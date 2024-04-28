# banana-tag

## How to run the code:
NOTE: For the lowest input delay, clone the repo locally and run the game on 
your own network: Noah-Terve/banana-tag (github.com), so that displays can pop up 
without using X-11 forwarding. If on Halligan, try to use joingame.sh on the same 
vm where the server is located to minimize delay.

First, you will need to install erpy and pygame on a python version 
that is able to run them (>= 3.11)

In order to run the game you will have to fire up a server node, 
you can do this by running ‘sh startserver.sh’

Once the server is running, on the same network any number of players 
can join by running ‘sh joingame.sh’. This join script will have the players 
enter the node name of the server and their name. It is important to have all 
players have unique names.

Once you have done both these steps the players will have 2 windows open, 
1 is a pygame window which is purely for visual, and the other will be their 
terminal that they ran the script from, this will be where you put input. 
Bring the terminal into focus, and then each player just has to press r to start the game.

Once the game is started you will move around with WASD, and when you tag 
another player you will click r p, or z to choose rock paper or scissors, 
whoever wins will stay in, and the other person will be out until the person
they lost to is back in.


## File Information

Below is a description of each file and what it is used for

client_display.py: Spins up the pygame window that players will see the 
game running on. This Python process will be spawned by the listen.erl 
module when the listen module opens an Erlang port. The process will 
then take incoming messages from the port and update the client display.

listen.erl: Erlang node that listens for communication from the server 
and then forwards it to the Python display on the client side. It will
spawn the Python process client_display.py through a port opening.

input.erl: The Erlang node that collects player input from the 
terminal and sends it to the server.

gameserver.erl: The Erlang node that sits underneath the Python server 
and handles message passing to and from clients. This file will spawn the 
server.py process by opening an Erlang port. It will pass incoming messages 
from clients to the Python server and also take messages from the Python server 
and send them to the clients.

server.py: Python server, handles all the processing in the game. Will communicate 
with gameserver.erl to pass messages to players and take updates from players as well.

joingame.sh: Script for clients to run to connect to the game. 
Will start the listen node and input node on the client side. 
This spawned listen node will in turn start client_display.py.

startserver.sh: Script to start the Erlang server node, which in turn, 
starts the server.py file
