# has to be on vm05 for now
erlc gameserver.erl
erl -sname server -run gameserver start