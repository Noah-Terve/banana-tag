# has to be on vm05 for now
erlc gameserver.erl
erl -sname server -setcookie secretCookie -run gameserver start '/usr/bin/python3.11 -u server.py'