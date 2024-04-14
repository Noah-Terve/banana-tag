from erpy import stdio_port_connection
from term import codec, Atom
import time

INBOX, PORT = stdio_port_connection()
PORT.send(Atom("go"))

# x = [(12, 16, "ghe"), (5, 6)]
# x = [(12, 15, 1, "hello")]
x = [(12, 15, 1, "hello"), (1, 2, 0, "goodbye")]
time.sleep(20)
# (Atom("update"), codec.term_to_binary(x))
# {update, 1234455677}
# Port.send 
# 19843847234872398472
PORT.send((Atom("update"), codec.term_to_binary(x)))


# python interpreter output of using codec
# >>> x = codec.term_to_binary([(12, 15, 1, "hello"), (1, 2, 0, "goodbye")])
# >>> x
# b'\x83l\x00\x00\x00\x02h\x04a\x0ca\x0fa\x01k\x00\x05helloh\x04a\x01a\x02a\x00k\x00\x07goodbyej'
# >>> (val, tail) = codec.binary_to_term(x)
# >>> val
# [(12, 15, 1, 'hello'), (1, 2, 0, 'goodbye')]

