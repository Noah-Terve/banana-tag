from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import time
import pygame
import queue
import threading
from datetime import datetime
from random import randrange as rr
from math import sqrt
from enum import Enum
import logging
logger = logging.getLogger(__name__)
logging.basicConfig(filename='server.log', encoding='utf-8', level=logging.DEBUG)

from erpy import stdio_port_connection
from term import codec, Atom

INBOX, PORT = stdio_port_connection()
PORT.send(Atom("go"))

class status(Enum):
    FREE = 0
    TAGGED = 1
    CONTENTION = 2

class choices(Enum):
    NONE = 0
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

# Global constants
DISPLAY_WIDTH = 1280
DISPLAY_HEIGHT = 720
RADIUS = 40
MOVE_SPEED = 2
DIRECTIONS = {97: [-MOVE_SPEED, 0], 115: [0, -MOVE_SPEED], 100: [MOVE_SPEED, 0], 119: [0, MOVE_SPEED]}
INPUTS = [97, 115, 100, 119]
CHOICE_MAP = {114: choices.ROCK, 112: choices.PAPER, 122: choices.SCISSORS}
CHOICES = [114, 112, 122]
DONE = False
CONTENTION_TIME = 8 # time that players spend playing rps in seconds
INPUT_QUEUE = queue.Queue()

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
        self.in_contention_with = None
        self.contention_time = None
        self.same_choice = False
        self.pos = pygame.Vector2(rr(RADIUS + 1, DISPLAY_WIDTH - RADIUS + 1), rr(RADIUS + 1, DISPLAY_HEIGHT - RADIUS + 1))
        self.color = make_new_rand_color()
        self.input = []
        self.choice = choices.NONE
    
    # things to do for updating
    # 1) 2 players are connected
    # 2) the player who got a player out is now out
    # 3) the players in a rps battle hit their time limit (resolve the battle)
    # 4) move the player if they are free
    def update(self):
        # check if you should be back in
        if self.status == status.TAGGED:
            if self.got_me_out.status != status.TAGGED:
                return
            self.status = status.FREE
            self.got_me_out = None
        
        # check collisions
        collision, other_player = check_collisions(self, GS.players)
        if collision:
            now = datetime.now()
            self.status = status.CONTENTION
            self.in_contention_with = other_player
            self.contention_time = now
            
            other_player.status = status.CONTENTION
            other_player.in_contention_with = self
            other_player.contention_time = now
        
        # process inputs
        for val in self.input:
            if val in INPUTS and self.status == status.FREE:
                x , y = DIRECTIONS[val]
                new_x = self.pos.x + x
                if new_x > RADIUS and new_x < DISPLAY_WIDTH - RADIUS:
                    self.pos.x = new_x
                
                new_y = self.pos.y + y
                if new_y > RADIUS and new_y < DISPLAY_HEIGHT - RADIUS:
                    self.pos.y = new_y
            elif val in CHOICES and self.status == status.CONTENTION:
                self.choice = CHOICE_MAP[val]
        self.input = []
        
        # check if we need to resolve contention.
        if self.status == status.CONTENTION and (datetime.now() - self.contention_time).seconds >= CONTENTION_TIME:
            # to resolve contention, we have to check both inputs, and compare them
            # then update each player's status, and reset the server side variables that
            # are important to have at certain values when entering contention
            my_choice = self.choice
            their_choice = self.in_contention_with.choice
            i_win = True
            if my_choice == their_choice:
                i_win = None
            elif their_choice == choices.NONE:
                pass # leave i_win as true
            elif my_choice == choices.ROCK and their_choice == choices.PAPER:
                i_win = False
            elif my_choice == choices.PAPER and their_choice == choices.SCISSORS:
                    i_win = False
            elif my_choice == choices.SCISSORS and their_choice == choices.ROCK:
                    i_win = None

            if i_win == None:
                self.same_choice = True
                self.in_contention_with.same_choice = True
            elif i_win:
                self.status = status.FREE
                self.in_contention_with = None
                self.choice = None
                self.same_choice = False
                
                self.in_contention_with.status = status.TAGGED
                self.in_contention_with.choice = None
                self.in_contention_with.same_choice = False
                self.in_contention_with.got_me_out = self
                self.in_contention_with.in_contention_with = None
            else:
                self.in_contention_with.status = status.FREE
                self.in_contention_with.in_contention_with = None
                self.in_contention_with.choice = None
                self.in_contention_with.same_choice = False
                
                self.status = status.TAGGED
                self.choice = None
                self.same_choice = False
                self.got_me_out = self.in_contention_with
                self.in_contention_with = None
        
# make a class which is the "game state" this will undoubtedly change as we
# progess, but just to have something. This stores coordinates for "players",
# those are the center of small circles which encompass the full player position
class game_state:
    def __init__(self) -> None:
        self.num_players = 0
        self.players = []
        
    def add_player(self, name):
        keys = self.players.keys()
        if name in keys:
            tag = 1
            while True:
                if not (name + str(tag)) in keys:
                    name = name + str(tag)
                    break
                tag += 1
        
        self.players.append(player(name))
        # somehow set the starting location of players


# global game state to be shared among proceses
GS = game_state()

def check_collisions(player, players):
    for other_player in players:
        if other_player == player or other_player.status != status.FREE:
            continue
        if dist(player.pos, other_player.pos) <= (RADIUS * 2):
            return True, other_player
    return False, None

def enqueue_input():
    for msg in INBOX:
        INPUT_QUEUE.put(msg)
        if msg == Atom("close"):
            break

# this is for when the game is starting
# collect players that want to join
def start_state():
    # TODO: while here wait until everyone is ready, and be waiting to accept
    #       people into the room
    
    # for each person who joins make a new player with their name, and
    # store the update, when everyone is ready send out the gs to everyone.
    pass


def run_game():
    running = True
    
    listener = threading.Thread(target=enqueue_input, args = [])
    listener.start()
    
    while running:
        try:
            msg = INPUT_QUEUE.get_nowait()
            if msg == Atom("close"):
                break
            
            logger.info(f"got {msg}")
            
            name, key = msg
            for player in GS.players:
                if player.name == name:
                    player.input.append(key)
        except:
            pass
            # no input to parse, do updates (this is primarily make sure no
            # timeouts were hit for people in contention)

        time.sleep(1)
        for player in GS.players:
            player.update()
            
        # send updates out to players
        # build list to send out
        to_send = []
        for player in GS.players:
            to_send.append((player.status, player.pos.x, player.pos.y, player.same_choice, player.got_me_out.name))
        PORT.send((Atom("update"),codec.term_to_binary(to_send)))

def main():
    start_state()
    run_game()


if __name__ == "__main__":
    main()
    