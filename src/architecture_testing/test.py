#!/usr/bin/env python3

from erpy import stdio_port_connection


def main():
    inbox, port = stdio_port_connection()
    while True:
        msg = input("Enter Message:")
        port.send(msg)


if __name__ == '__main__':
    main()