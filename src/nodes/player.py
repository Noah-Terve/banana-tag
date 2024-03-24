from erpy import stdio_port_connection

inbox, port = stdio_port_connection()
for i in range(100):
    port.send(i)