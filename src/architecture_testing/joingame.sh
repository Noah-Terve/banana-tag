# echo "Input your username:"
# read name
# echo $name
erl -sname client -noshell -run input connect test -eval 'init:stop()'
# erl -sname client -noshell -run input listen $name -eval
