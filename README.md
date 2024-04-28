# banana-tag

## How to run the code:
First, you will need to install erpy and pygame on a python version that
is able to run them (>= 3.11)

In order to run the game you will have to fire up a server node, you can
do this by running
`sh startserver.sh`

Once the server is running, on the same network any number of players can join
by running
`sh joingame.sh`

This join script will have the players enter the node name of the server and
their name. It is important to have all players have unique names. 

Once you have done both these steps the players will have 2 windows open, 1
is a pygame window which is purly for visual, and the other will be their
terminal that they ran the script from, this will be where you put input. 
Bring the terminal into focus, and then each player just has to press r to
start the game.

Once the game is started you will move around with wasd, and when you tag
another player you will click r p or z to choose rock paper or scissors,
whoever wins that will stay in, and the other person will be out until the
person they lost to is back in.

## File Information

Below is a description of each file and what it is used for

client_display.py: Spins up the pygame window that players will see the game
running on.

gameserver.erl: The erlang node that sits underneath the python server
and handles message passing to and from clients.

input.erl: The erlang node that collects player input and sends it to the
server.

joingame.sh: Script for clients to run to connect to the game.

listen.erl: Erlang node that listens for communication from the server and
then forwards it to the python display on the client side.

server.py: Python server, handles all the processing in the game.

startserver.sh: Script to start the python server.