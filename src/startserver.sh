# has to be on vm05 for now
erlc gameserver.erl
erl -sname server -run gameserver start '/usr/bin/python3.11 -u server.py'