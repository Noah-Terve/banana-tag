import pygame
import erpy
from .server import status, player, game_state, DISPLAY_HEIGHT, DISPLAY_WIDTH, RADIUS

def main():
    # TODO: Get game state from server
    gs = game_state(10)
    # TODO: Get the palyer that represents you
    me = 1
    screen = pygame.display.set_mode((DISPLAY_WIDTH, DISPLAY_HEIGHT))
    clock = pygame.time.Clock()
    running = True
    update = True
    
    while running:
        if update:
            pass
            # TODO: get updates from erlang
        
        else:
            # poll for events
            # pygame.QUIT event means the user clicked X to close your window
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

            # fill the screen with a color to wipe away anything from last frame
            # comment this out to have a fun lil adventure
            screen.fill("white")
            
            # this isn't perfect because players later in the list have an advantage
            for player in gs.players:
                if player.status:
                    pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                        
            pygame.display.flip()
            
        keys = pygame.key.get_pressed()
        if keys[pygame.K_w]:
            player_pos.y -= 300 * dt
        if keys[pygame.K_s]:
            player_pos.y += 300 * dt
        if keys[pygame.K_a]:
            player_pos.x -= 300 * dt
        if keys[pygame.K_d]:
            player_pos.x += 300 * dt

    # TODO: send message to server telling it we are done.

if __name__ == "__main__":
    main()
    