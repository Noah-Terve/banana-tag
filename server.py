# server.py
# Python server, handles all the processing in the game.

from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import sys
import pygame
import queue
import threading
from datetime import datetime
from random import randrange as rr
from math import sqrt
import logging
logger = logging.getLogger(__name__)
logging.basicConfig(filename='Server.log', encoding='utf-8', level=logging.DEBUG)

from erpy import stdio_port_connection
from term import codec, Atom

MSG_NUM = 0

# tell the erlang we are good to go
INBOX, PORT = stdio_port_connection()
PORT.send(Atom("go"))

# status options
FREE = 0
TAGGED = 1
CONTENTION = 2
WAITING = 3

# player choice options
NONE = 0
ROCK = 1
PAPER = 2
SCISSORS = 3

# Global constants
DISPLAY_WIDTH = 1280
DISPLAY_HEIGHT = 720
RADIUS = 40
MOVE_SPEED = 7
DIRECTIONS = {97: [-MOVE_SPEED, 0], 115: [0, MOVE_SPEED], 100: [MOVE_SPEED, 0], 119: [0, -MOVE_SPEED]}
INPUTS = [97, 115, 100, 119]
CHOICE_MAP = {114: ROCK, 112: PAPER, 122: SCISSORS}
CHOICES = [114, 112, 122]
CONTENTION_TIME = 8 # time that players spend playing rps in seconds

# global variables
INPUT_QUEUE = queue.Queue()
PLAYERS = []
DONE = False
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
    """get the distance between 2 points

    Args:
        pos1 (pygame position): first position
        pos2 (pygame position): second position

    Returns:
        float: distance between the 2 passed in points
    """
    return sqrt((abs(pos1.x - pos2.x)**2) + (abs(pos1.y - pos2.y)**2))


class Player:
    """
        Player class which stores information about players and allows them
        to update themselves.
    """
    def __init__(self, name) -> None:
        """Initialize a player.

        Args:
            name (string): name of player.
        """
        self.name = name
        self.status = WAITING
        self.got_me_out = None
        self.in_contention_with = None
        self.contention_time = None
        self.same_choice = False
        self.pos = pygame.Vector2(rr(RADIUS + 1, DISPLAY_WIDTH - RADIUS + 1),
                                  rr(RADIUS + 1, DISPLAY_HEIGHT - RADIUS + 1))
        self.color = make_new_rand_color()
        self.input = []
        self.choice = NONE
    
    # things to do for updating
    # 1) 2 players are connected
    # 2) the player who got a player out is now out
    # 3) the players in a rps battle hit their time limit (resolve the battle)
    # 4) move the player if they are free
    def update(self):
        """Update the player's info based on their input.

        Returns:
            bool: _description_
        """
        updating = False
        if self.input != []:
            updating = True
        # check if you should be back in
        if self.status == TAGGED:
            if self.got_me_out.status != TAGGED:
                self.input = []
                return updating
            self.status = FREE
            self.got_me_out = None
            updating = True
        
        # check collisions
        collision, other_player = check_collisions(self, PLAYERS)
        if collision:
            now = datetime.now()
            self.status = CONTENTION
            self.in_contention_with = other_player
            self.contention_time = now
            
            other_player.status = CONTENTION
            other_player.in_contention_with = self
            other_player.contention_time = now
            
            updating = True
        
        # process inputs
        for val in self.input:
            if val in INPUTS and self.status == FREE:
                x , y = DIRECTIONS[val]
                new_x = self.pos.x + x
                if new_x > RADIUS and new_x < DISPLAY_WIDTH - RADIUS:
                    self.pos.x = new_x
                
                new_y = self.pos.y + y
                if new_y > RADIUS and new_y < DISPLAY_HEIGHT - RADIUS:
                    self.pos.y = new_y
            elif val in CHOICES and self.status == CONTENTION:
                self.choice = CHOICE_MAP[val]
        self.input = []
        
        # check if we need to resolve contention.
        if self.status == CONTENTION and \
           (datetime.now() - self.contention_time).seconds >= CONTENTION_TIME:
            # to resolve contention, we have to check both inputs, and compare them
            # then update each player's status, and reset the server side variables that
            # are important to have at certain values when entering contention
            my_choice = self.choice
            their_choice = self.in_contention_with.choice
            i_win = True
            if my_choice == their_choice:
                i_win = None
            elif their_choice == NONE:
                pass # leave i_win as true
            elif my_choice == ROCK and their_choice == PAPER:
                i_win = False
            elif my_choice == PAPER and their_choice == SCISSORS:
                    i_win = False
            elif my_choice == SCISSORS and their_choice == ROCK:
                    i_win = None

            if i_win == None:
                self.same_choice = True
                self.in_contention_with.same_choice = True
            elif i_win:
                self.in_contention_with.status = TAGGED
                self.in_contention_with.choice = None
                self.in_contention_with.same_choice = False
                self.in_contention_with.got_me_out = self
                self.in_contention_with.in_contention_with = None
                
                self.status = FREE
                self.in_contention_with = None
                self.choice = None
                self.same_choice = False
            else:
                self.in_contention_with.status = FREE
                self.in_contention_with.in_contention_with = None
                self.in_contention_with.choice = None
                self.in_contention_with.same_choice = False
                
                self.status = TAGGED
                self.choice = None
                self.same_choice = False
                self.got_me_out = self.in_contention_with
                self.in_contention_with = None
            updating = True
        return updating

def check_collisions(player, players):
    """check if a player has collided with anyone else.

    Args:
        player (Player): Player to check collisions for.
        players (List Players): List of all other players.

    Returns:
        (Bool, Player): Returns whether the player has collided with anyone,
                        and if so, which player that is.
    """
    for other_player in players:
        if other_player == player or other_player.status != FREE:
            continue
        if dist(player.pos, other_player.pos) <= (RADIUS * 2):
            return True, other_player
    return False, None

def enqueue_input():
    """
    Listen for input from the erlang and put it into the queue for the
        display to pick up.
    """
    global INPUT_QUEUE
    for msg in INBOX:
        logger.info(f"Got message: {msg} putting it on the queue")
        INPUT_QUEUE.put(msg)
        if msg == Atom("close"):
            break

def all_ready():
    """check if all players are ready.

    Returns:
        bool: indication of whether all players are ready or not.
    """
    for player in PLAYERS:
        if player.status != FREE:
            return False
    return True

# this is for when the game is starting
# collect players that want to join
def start_state():
    """
        Sit in the start state and allow player connections until at least 2
        players join, and then everyone is ready.

    Returns:
        bool: indication of whether or not the game should start or we are
              resetting the server 
    """
    global PLAYERS
    global MSG_NUM
    global INPUT_QUEUE
    # wait for at least 2 people to join and ready
    while not all_ready() or len(PLAYERS) < 2:
        msg = INPUT_QUEUE.get()
        logger.info(f"got message: {msg}")
        if msg == Atom("close"):
            sys.exit(0)
        
        if msg == Atom("restart"):
            return False
        
        if type(msg) == type(""): # if msg is string palyer is connecting
            PLAYERS.append(Player(msg))
            
        else: # player sending data
            name, key = msg
            if key == 114:
                for player in PLAYERS:
                    if player.name == name:
                        player.status = FREE

    # send a message to everyone that we are starting, and include all data
    # they need to initialize their players
    to_send = []
    for player in PLAYERS:
        to_send.append((player.name, player.pos.x, player.pos.y, player.color))
    logger.info(f"Message {MSG_NUM}, at {datetime.now()}: {to_send}")
    MSG_NUM += 1
    PORT.send((Atom("update"),codec.term_to_binary(to_send)))
    return True


def run_game():
    """
        Run the game by looping and getting input and then updating the state
        of the game.
    """
    global PLAYERS
    global MSG_NUM
    global INPUT_QUEUE
    while True:
        # get 10 inputs
        for _ in range(10):
            try:
                msg = INPUT_QUEUE.get_nowait()
                if msg == Atom("close"):
                    sys.exit(0)
                
                if msg == Atom("restart"):
                    return
                
                logger.info(f"got {msg}")
                try:
                    name, key = msg
                    for player in PLAYERS:
                        if player.name == name:
                            player.input.append(key)
                except:
                    pass # case of player connecting, don't do anything for them
            except:
                pass
                # no input to parse, do updates (this is primarily make sure no
                # timeouts were hit for people in contention)

        updating = False
        for player in PLAYERS:
            updating = player.update() or updating
            
        # send updates out to players
        # build list to send out
        to_send = []
        for player in PLAYERS:
            if player.got_me_out:
                to_send.append((player.status, player.pos.x, player.pos.y,
                                player.same_choice, player.got_me_out.name))
            elif player.in_contention_with:
                to_send.append((player.status, player.pos.x, player.pos.y,
                                player.same_choice, 
                                player.in_contention_with.name))
            else:
                to_send.append((player.status, player.pos.x, player.pos.y,
                                player.same_choice, ""))
        
        if updating:
            logger.info(f"Message {MSG_NUM} at {datetime.now()}: {to_send}")
            MSG_NUM += 1
            PORT.send((Atom("update"),codec.term_to_binary(to_send)))

def main():
    """Run the gameserver.
    """
    global PLAYERS
    listener = threading.Thread(target=enqueue_input, args = [])
    listener.start()
    while True:
        PLAYERS = []
        if not start_state():
            continue
        run_game()

if __name__ == "__main__":
    main()