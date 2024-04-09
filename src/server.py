from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import sys
from time import sleep
import pygame
from random import randrange as rr
from math import sqrt
from enum import Enum

from erpy import stdio_port_connection
from term import Atom

INBOX, PORT = stdio_port_connection()
PORT.send(Atom("go"))
#print("successful connection", file=sys.stderr, flush=True)

# Global constants
DISPLAY_WIDTH = 1280
DISPLAY_HEIGHT = 720
RADIUS = 40
MOVE_SPEED = 2
DIRECTIONS = {"a": [-MOVE_SPEED, 0], "s": [0, -MOVE_SPEED], "d": [MOVE_SPEED, 0], "w": [0, MOVE_SPEED]}

class status(Enum):
    FREE = 0
    TAGGED = 1
    CONTENTION = 2

# these are status, might want to add one for in contact while they are figuring
# out who wins
COLORS = []

def make_new_rand_color():
    """
    Generate a random color that is not already in the colors list.
    
    Returns:
        (int, int, int): The random color that was generated, and is guarunteed
        to not be in the list of colors.
    """
    while True:
        color = (rr(255), rr(255), rr(255))
        if color not in COLORS and color != (255, 255, 255) and color != (0, 0, 0):
            COLORS.append(color)
            return color

def dist(pos1, pos2):
    return sqrt((abs(pos1.x - pos2.x)**2) + (abs(pos1.y - pos2.y)**2))


class player:
    def __init__(self, name) -> None:
        self.name = name
        self.status = status.FREE
        self.got_me_out = None
        self.pos = pygame.Vector2(rr(RADIUS + 1, DISPLAY_WIDTH - RADIUS + 1), rr(RADIUS + 1, DISPLAY_HEIGHT - RADIUS + 1))
        self.color = make_new_rand_color()
        self.input = []
    
    def move(self):
        for val in self.input:
            x , y = DIRECTIONS[val]
            new_x = self.pos.x + x
            if new_x > RADIUS and new_x < DISPLAY_WIDTH - RADIUS:
                self.pos.x = new_x
            
            new_y = self.pos.y + y
            if new_y > RADIUS and new_y < DISPLAY_HEIGHT - RADIUS:
                self.pos.y = new_y

# make a class which is the "game state" this will undoubtedly change as we
# progess, but just to have something. This stores coordinates for "players",
# those are the center of small circles which encompass the full player position
class game_state:
    def __init__(self, num_players) -> None:
        self.num_players = num_players
        self.players = [0]*num_players
        # somehow set the starting location of players
        
        # for now just do something random
        i = 0
        for _ in self.players:
            # for use with pygame
            # gives a name, current status, who got you out, position, and color to each player
            # You'll only need to send out the status and position with updates,
            # name and color don't change
            self.players[i] = player("Player " + str(i))
            i += 1
        
        # print(self.players)

def check_collisions(player, players):
    for other_player in players:
        if other_player == player or other_player.status != status.FREE:
            continue
        if dist(player.pos, other_player.pos) <= (RADIUS * 2):
            return True, other_player
    return False, None

def collide(p1, p2):
    p1.status = status.CONTENTION
    p2.status = status.CONTENTION
    # TODO: may have to send updates here, or start some clock attached to the
    #       players which counts down.. we can figure out how long this needs to
    #       be from testing the actual latency


def move(pos):
    # TODO: This needs to get updated when we decide what the input from the
    #       player actually looks like and how we get that info.
    x , y = DIRECTIONS[rr(4)]
    new_x = pos.x + x
    if new_x > RADIUS and new_x < DISPLAY_WIDTH - RADIUS:
        pos.x = new_x
    
    new_y = pos.y + y
    if new_y > RADIUS and new_y < DISPLAY_HEIGHT - RADIUS:
        pos.y = new_y

# this is for when the game is starting
# collect players that want to join
def start_state(gs):
    # TODO: while here wait until everyone is ready, and be waiting to accept
    #       people into the room
    
    # for each person who joins make a new player with their name, and
    # store the update, when everyone is ready send out the gs to everyone.
    pass


def game_running(gs):
    running = True
    update = True
    
    with open("output.txt", "a") as f:
        print("In Game running", file=f, flush=True)
    
    while running:
        # TODO: one thing to figure out is when we send updates
        if update:
            with open("output.txt", "a") as f:
                print("In update right now", file=f, flush=True)
            # grab updates on what players are doing from erlang
            i = 0
            # TODO: this will block until it actually gets 10 messages, need
            #       to add a timeout 
            for msg in INBOX:
                with open("output.txt", "a") as f:
                    print(f"In inbox, iter {i}", file=f, flush=True)
                
                if (i == 10):
                    break
                
                if msg == Atom("close"):
                    with open("output.txt", "a") as f:
                        print(f"Closing", file=f, flush=True)
                    return
                
                with open("output.txt", "a") as f:
                    print(f"got {msg}, type: {type(msg)}, {dir(msg)}", file=f, flush=True)

                i += 1
                
            update = False
        
        else:
            with open("output.txt", "a") as f:
                print("In else to perform updates right now", file=f, flush=True)
            for player in gs.players:
                # for every player check if their status should change
                # should change if:
                # 1) 2 players are connected
                # 2) the player who got a player out is now out 
                #       - this might be better done when we resolve rps, 
                #         but then we'd have to loop through the whole
                #         list when we do that which might be peromance inhibiting
                #       (especially since this is already squared time because of
                #        the collision checking.. there may be a way to optimize
                #        the checking of collisions with multiple threads.. I'm
                #        thinking about it if we need to, but hopefully we don't have to)
                # 3) the players in a rps battle hit their time limit (resolve the battle)
                
                # check if you should be back in
                if player.status == status.TAGGED:
                    if player.got_me_out.status == status.FREE:
                        continue
                    # print("Because " + player.got_me_out.name + " got out, " + player.name + " is back in")
                    player.status = status.FREE
                    player.got_me_out = None
                    
                # check collisions
                collision, other_player = check_collisions(player, gs.players)
                if collision:
                    collide(player, other_player)
                
                if player.status == status.FREE:
                    player.move()
            update = True

def main():
    gs = game_state(0)
    start_state(gs)
    game_running(gs)
    with open("output.txt", "a") as f:
        print(f"Done", file=f, flush=True)


if __name__ == "__main__":
    main()
    