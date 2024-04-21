#!/bin/sh

echo "Input your username:"
read name
# echo $name

erlc input.erl
erlc listen.erl

inputnode="${name}-input"
listennode="${name}-listen" 


# start listen node first
erl -sname $listennode -setcookie secretCookie -hidden -noshell -run listen start $name '/usr/bin/python3.11 client_display.py'&

# prep terminal for input
stty --f /dev/tty icanon raw
erl -sname $inputnode -setcookie secretCookie -hidden -noshell -run input start $name -run init stop
# set terminal back 
stty echo echok icanon -raw
