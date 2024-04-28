# Client_display.py
# Runs the display on the client side

from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import sys
import pygame
import queue
import threading
from term import codec, Atom
import erpy

INBOX, PORT = erpy.stdio_port_connection()
PLAYERS = []

# status options
FREE = 0
TAGGED = 1
CONTENTION = 2

# Global constants
DISPLAY_WIDTH = 1280
DISPLAY_HEIGHT = 720
RADIUS = 40

class Player:
    """Info that client needs on the player."""
    def __init__(self, name, x, y, color) -> None:
        self.name = name
        self.status = FREE
        self.got_me_out = None
        self.same_choice = False
        self.pos = pygame.Vector2(x, y)
        self.color = color

# set up the input queue and pygame
INPUT_QUEUE = queue.Queue()
screen = pygame.display.set_mode((DISPLAY_WIDTH, DISPLAY_HEIGHT))
pygame.display.set_caption('Banana Tag')
pygame.font.init()
my_font = pygame.font.Font('freesansbold.ttf', 50)
player_font = pygame.font.Font('freesansbold.ttf', 20)

def enqueue_input():
    """
        Listen for input from the erlang and put it into the queue for the
        display to pick up.
    """
    for msg in INBOX:
        INPUT_QUEUE.put(msg)
        if msg == Atom("close"):
            break

def pre_game():
    """
        Before the game starts bring up the pygame window and display initial
        text to player.
    """
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return
        
        screen.fill("white")
        text_surface = \
            my_font.render('Waiting for game to start', False, (0, 0, 0))
        textRect = text_surface.get_rect()
        textRect.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 - 50)
        screen.blit(text_surface, textRect)
        
        text_surface2 = \
            my_font.render('Press r to signal ready', False, (0, 0, 0))
        textRect2 = text_surface2.get_rect()
        textRect2.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 + 50)
        
        screen.blit(text_surface2, textRect2)
        pygame.display.flip()
        
        try:
            msg = INPUT_QUEUE.get_nowait()
            if msg == Atom("close"):
                sys.exit(0)
            
            if type(msg) == type([]): # game server sent out info to start game
                for player in msg:
                    # each player is a tuple so unpack it into the constructor
                    PLAYERS.append(Player(*player))
                return
        except:
            pass

def main():
    """
        Run the player display, get the pregame set up and then run the game
        loop once we have info from the server. All this does is display info.
    """
    listener = threading.Thread(target=enqueue_input, args = [])
    listener.start()
    
    name = INPUT_QUEUE.get()
    pre_game()
    this_player = None
    for player in PLAYERS:
        if player.name == name:
            this_player = player
            break
    
    if not this_player:
        return
    
    while True:
        # poll for events
        # pygame.QUIT event means the user clicked X to close your window
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                PORT.send(Atom("close"))
                return
        
        # get 50 messages, or keep going
        for _ in range(50):
            try:
                msg = INPUT_QUEUE.get_nowait()
                if msg == Atom("close"):
                    return
                
                # game server sent updated player info
                if type(msg) == type([]):
                    i = 0
                    for status, x, y, same_choice, got_me_out in msg:
                        PLAYERS[i].status = status
                        PLAYERS[i].pos.x = x
                        PLAYERS[i].pos.y = y
                        PLAYERS[i].same_choice = same_choice
                        if got_me_out:
                            PLAYERS[i].got_me_out = got_me_out
                        else:
                            PLAYERS[i].got_me_out = ""
                        i += 1
            except:
                pass
        
        if this_player.status == TAGGED:
            screen.fill("white")
                
            text_surface = my_font.render('You were tagged by ' +
                                          f'{this_player.got_me_out}.',
                                          False, (0, 0, 0))
            textRect = text_surface.get_rect()
            textRect.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 + 100)
            screen.blit(text_surface, textRect)
                
            text_surface2 = my_font.render('Waiting for them to get out.',
                                           False, (0, 0, 0))
            textRect2 = text_surface2.get_rect()
            textRect2.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 + 200)
            screen.blit(text_surface2, textRect2)
            
            for player in PLAYERS:
                if player.status == FREE:
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                    text_surface = player_font.render(f'{player.name}', 
                                                      False, (0, 0, 0))
                    textRect = text_surface.get_rect()
                    textRect.center = (player.pos.x, player.pos.y)
            
        elif this_player.status == CONTENTION:
            screen.fill("white")
            
            text_surface = my_font.render(f'You and {this_player.got_me_out}' +
                                          'tagged eachother!', False, (0, 0, 0))
            textRect = text_surface.get_rect()
            textRect.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 - 50)
            screen.blit(text_surface, textRect)
            
            text_surface2 = my_font.render('Press r, p, or z to choose rock' +
                                           'paper or scissors', False, 
                                           (0, 0, 0))
            textRect2 = text_surface2.get_rect()
            textRect2.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 + 50)
            screen.blit(text_surface2, textRect2)
            
        elif this_player.status == FREE:
            screen.fill("white")
                
            for player in PLAYERS:
                if player.status == FREE:
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                    text_surface = player_font.render(f'{player.name}',
                                                      False, (0, 0, 0))
                    textRect = text_surface.get_rect()
                    textRect.center = (player.pos.x, player.pos.y)
                    screen.blit(text_surface, textRect)

        pygame.display.flip()
    
if __name__ == "__main__":
    main()
    