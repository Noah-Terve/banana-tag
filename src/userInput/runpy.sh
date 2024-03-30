#!/bin/sh
stty --f /dev/tty icanon raw
erl -pa ./ -run input start -run init stop -noshell
stty echo echok icanon -raw