# startserver.sh
# Scrpt to start the server, spawn the erlang node that then spawns the python.

erlc gameserver.erl
erl -sname server -setcookie secretCookie -run gameserver start '/usr/local/bin/python3.11 -u server.py'