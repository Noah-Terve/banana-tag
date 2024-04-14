from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import pygame
import queue
import threading
from term import codec, Atom
import erpy
import logging
logger = logging.getLogger(__name__)
logging.basicConfig(filename='clientDisplay.log', encoding='utf-8', level=logging.DEBUG)

INBOX, PORT = erpy.stdio_port_connection()


from server import status, player, game_state, DISPLAY_HEIGHT, DISPLAY_WIDTH, RADIUS

INPUT_QUEUE = queue.Queue()
screen = pygame.display.set_mode((DISPLAY_WIDTH, DISPLAY_HEIGHT))
pygame.display.set_caption('Banana Tag')
pygame.font.init()
my_font = pygame.font.Font('freesansbold.ttf', 50)

def enqueue_input():
    for msg in INBOX:
        logger.info(f"message: {msg} recieved, putting it in the queue")
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
        
        # TODO: replace with some version of wait for the server to say we are a go
        keys = pygame.key.get_pressed()
        if keys[pygame.K_r]:
            return


def main():
    
    # pre_game()
    # TODO: Get game state from server
    gs = game_state()
    
    # TODO: Get the palyer that represents you
    me = 1

    listener = threading.Thread(target=enqueue_input, args = [])
    listener.start()
    
    running = True
    i = 0
    while running:
        try:
            msg = INPUT_QUEUE.get_nowait()
            if msg == Atom("close"):
                break
            
            logger.info(f"got {msg}")

        except:
            pass
        # TODO: based on the current player's state we could change the
        #       display as well.
        
        # poll for events
        # pygame.QUIT event means the user clicked X to close your window
        # for event in pygame.event.get():
        #     if event.type == pygame.QUIT:
        #         running = False

        # # fill the screen with a color to wipe away anything from last frame
        # screen.fill("white")
        
        # for player in gs.players:
        #     if player.status == status.FREE:
        #         pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                    
        # pygame.display.flip()

    # TODO: send message to erlang telling it we are done.

if __name__ == "__main__":
    main()
    