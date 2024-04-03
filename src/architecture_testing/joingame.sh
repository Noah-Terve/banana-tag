#!/bin/sh

echo "Input your username:"
read name
# echo $name

erlc input.erl

# start listen node first
erl -sname client -noshell -run listen connect $name -run init stop

# prep terminal for input
stty --f /dev/tty icanon raw

# erl -pa ./ -run input start -run init stop -noshell
erl -sname client -noshell -run input start $name -run init stop

# set terminal back 
stty echo echok icanon -raw
