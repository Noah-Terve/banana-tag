echo "Input the path to your python"
read python


erlc gameserver.erl
erl -sname server -setcookie secretCookie -run gameserver start '/usr/local/bin/python3.11 -u server.py'