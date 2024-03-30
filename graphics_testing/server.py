import pygame
from random import random, randrange as rr
from math import sqrt
from enum import Enum

class status(Enum):
    FREE = 0
    TAGGED = 1
    CONTENTION = 2


# Global constants
DISPLAY_WIDTH = 1280
DISPLAY_HEIGHT = 720
RADIUS = 40
RAND_MOVE_SPEED = 5
DIRECTIONS = [[-RAND_MOVE_SPEED, 0], [0, -RAND_MOVE_SPEED], [RAND_MOVE_SPEED, 0], [0, RAND_MOVE_SPEED]]

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
        
        print(self.players)

def check_collisions(player, players):
    for other_player in players:
        if other_player == player or not other_player.status:
            continue
        if dist(player.pos, other_player.pos) <= (RADIUS * 2):
            return True, other_player
    return False, None

def collide(p1, p2):
    if random() > 0.5:
        p2.status = status.TAGGED
        p2.got_me_out = p1
        print(p1.name + " tagged " + p2.name)
    else:
        p1.status = status.TAGGED
        p1.got_me_out = p2
        print(p2.name + " tagged " + p1.name) 

def move(pos):
    x , y = DIRECTIONS[rr(4)]
    new_x = pos.x + x
    if new_x > RADIUS and new_x < DISPLAY_WIDTH - RADIUS:
        pos.x = new_x
    
    new_y = pos.y + y
    if new_y > RADIUS and new_y < DISPLAY_HEIGHT - RADIUS:
        pos.y = new_y
        

def main():
    gs = game_state(25)
    screen = pygame.display.set_mode((DISPLAY_WIDTH, DISPLAY_HEIGHT))
    clock = pygame.time.Clock()
    running = True
    
    while running:
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
            if player.status == status.TAGGED:
                # check if you should be back in
                if player.got_me_out.status:
                    continue
                print("Because " + player.got_me_out.name + " got out, " + player.name + " is back in")
                player.status = status.FREE
                player.got_me_out = None
                
            # check collisions
            collision, other_player = check_collisions(player, gs.players)
            if collision:
                collide(player, other_player)
            
            if player.status == status.FREE:
                pygame.draw.circle(screen, player.color, player.pos, RADIUS)
                move(player.pos)         
                        
        # flip() the display to put your work on screen
        pygame.display.flip()

        # limits FPS to 60
        # dt is delta time in seconds since last frame, used for framerate-
        # independent physics.
        # dt = clock.tick(60) / 1000
    

if __name__ == "__main__":
    main()
    