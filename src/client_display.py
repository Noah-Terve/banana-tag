from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import sys
import pygame
import queue
import threading
from term import codec, Atom
import erpy
import logging
from datetime import datetime

# logger = logging.get# logger(__name__)
# logging.basicConfig(filename='Display.log', encoding='utf-8', level=logging.DEBUG)

END = 0
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
    def __init__(self, name, x, y, color) -> None:
        self.name = name
        self.status = FREE
        self.got_me_out = None
        self.same_choice = False
        self.pos = pygame.Vector2(x, y)
        self.color = color

INPUT_QUEUE = queue.Queue()
screen = pygame.display.set_mode((DISPLAY_WIDTH, DISPLAY_HEIGHT))
pygame.display.set_caption('Banana Tag')
pygame.font.init()
my_font = pygame.font.Font('freesansbold.ttf', 50)

def enqueue_input():
    MSG_NUM = 0
    for msg in INBOX:
        if END:
            # logger.info("Python is dying, killing python listener")
            break
        # logger.info(f"Message {MSG_NUM} at {datetime.now()}: {msg}")
        MSG_NUM += 1
        # # logger.info(f"message: {msg} recieved, putting it in the queue")
        INPUT_QUEUE.put(msg)
        if msg == Atom("close"):
            break

def pre_game():
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return
        
        screen.fill("white")
        text_surface = my_font.render('Waiting for game to start', False, (0, 0, 0))
        textRect = text_surface.get_rect()
        textRect.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 - 50)
        screen.blit(text_surface, textRect)
        
        # TODO: this may be misleading, might want some instructions somewhere
        text_surface2 = my_font.render('Press r to signal ready', False, (0, 0, 0))
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
                    PLAYERS.append(Player(*player))
                # logger.info(PLAYERS)
                return
        except:
            pass


def main():
    listener = threading.Thread(target=enqueue_input, args = [])
    listener.start()
    
    name = INPUT_QUEUE.get()
    # logger.info(f"Player name is {name}")
    pre_game()
    this_player = None
    for player in PLAYERS:
        if player.name == name:
            this_player = player
            break
    
    if not this_player:
        # logger.info("failed to get player information based on name, exiting")
        return
    
    while True:
        # poll for events
        # pygame.QUIT event means the user clicked X to close your window
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return
        
        for _ in range(50):
            try:
                msg = INPUT_QUEUE.get_nowait()
                if msg == Atom("close"):
                    break
                
                if type(msg) == type([]): # game server sent out info to start game
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
                
            text_surface = my_font.render(f'You were tagged by {this_player.got_me_out}. Waiting for them to get out.', False, (0, 0, 0))
            textRect = text_surface.get_rect()
            textRect.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 - 50) #TODO edit placement
            screen.blit(text_surface, textRect)
                
            for player in PLAYERS:
                if player.status == FREE:
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)
            
        elif this_player.status == CONTENTION:
            screen.fill("white")
            
            text_surface = my_font.render(f'You and {this_player.got_me_out} tagged eachother!', False, (0, 0, 0))
            textRect = text_surface.get_rect()
            textRect.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 - 50)
            screen.blit(text_surface, textRect)
            
            text_surface2 = my_font.render('Press r, p, or z to choose rock paper or scissors', False, (0, 0, 0))
            textRect2 = text_surface2.get_rect()
            textRect2.center = (DISPLAY_WIDTH // 2, DISPLAY_HEIGHT // 2 + 50)
            screen.blit(text_surface2, textRect2)
            
        elif this_player.status == FREE:
            # fill the screen with a color to wipe away anything from last frame
            screen.fill("white")
                
            for player in PLAYERS:
                if player.status == FREE:
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)

        pygame.display.flip()
    
    # TODO: send message to erlang telling it we are done.

if __name__ == "__main__":
    main()
    