from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import pygame
import erpy

# Below is for venv use
import sys
# NOTE, you need to change this to your local path to the graphics_testing dir
sys.path.append("/h/czhou06/comp21/project/banana-tag/src/graphics_testing/")
from server import status, player, game_state, DISPLAY_HEIGHT, DISPLAY_WIDTH, RADIUS

# Below for not venv use
# from .server import status, player, game_state, DISPLAY_HEIGHT, DISPLAY_WIDTH, RADIUS


screen = pygame.display.set_mode((DISPLAY_WIDTH, DISPLAY_HEIGHT))
pygame.display.set_caption('Banana Tag')
pygame.font.init()
my_font = pygame.font.Font('freesansbold.ttf', 50)

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
    
    pre_game()
    # TODO: Get game state from server
    gs = game_state(10)
    # TODO: Get the palyer that represents you
    me = 1

    running = True
    update = True
    inbox, port = erpy.stdio_port_connection()
    
    while running:
        if update:
            pass
            update = False
            # TODO: get updates from erlang
        
        else:
            # TODO: based on the current player's state we could change the
            #       display as well.
            
            # poll for events
            # pygame.QUIT event means the user clicked X to close your window
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

            # fill the screen with a color to wipe away anything from last frame
            screen.fill("white")
            
            for player in gs.players:
                if player.status == status.FREE:
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                        
            pygame.display.flip()
            update = True

    # TODO: send message to erlang telling it we are done.

if __name__ == "__main__":
    main()
    