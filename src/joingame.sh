#!/bin/sh

echo "Input your username:"
read name
# echo $name

echo "Input the node name of the server"
read server

# echo "Input the path to your python"
# read python

erlc input.erl
erlc listen.erl

inputnode="${name}-input"
listennode="${name}-listen" 
# pyth="${python} client_display.py"

# start listen node first
erl -sname $listennode -setcookie secretCookie -hidden -noshell -run listen start $server $name "/usr/local/bin/python3.11 client_display.py"&

# prep terminal for input
stty -f /dev/tty icanon raw
erl -sname $inputnode -setcookie secretCookie -hidden -noshell -run input start $server $name -run init stop
# set terminal back 
stty echo echok icanon -raw
