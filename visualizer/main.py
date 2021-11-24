import sys
import pygame
import time
import json
import os
import random
from pprint import pprint
import slice
import legend
from init import *
import systems
import files

screen = pygame.display.set_mode(size)


while True:
    for event in pygame.event.get():
        handle_event(event)

    """
    Updates and logic
    """
    # TODO (feature): add panning

    """
    Drawing
    """
    screen.fill((221, 231, 251))


    for s in slice.slices:
        s.draw(screen)
    systems.draw_system_rects(screen, files.locations)
        
    draw_grid(screen)
    draw_ranges(screen)

    systems.draw_system_labels(screen, files.locations)
    
    if ("hierarchic" in sys.argv[1]):
        legend.draw_legend(screen, slice.components)
    files.draw_file_indicators(screen)

    pygame.display.flip()
    time.sleep(0.1)
    files.try_load_pnr()
    files.try_load_locs()
