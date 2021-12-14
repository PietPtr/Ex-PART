import sys
import pygame
import time
from pprint import pprint

from init import *
import files
import slice
import legend
import systems
import grid
import routing
import connections

screen = pygame.display.set_mode(size)


while True:
    for event in pygame.event.get():
        handle_event(event, screen)

    """
    Updates and logic
    """
    mov = pygame.mouse.get_rel()
    if (pygame.mouse.get_pressed()[0]):
        zoom = (SQUARE_SIZE / BASE_SS)
        VIEW[0] += mov[0] * 1 / zoom
        VIEW[1] += mov[1] * 1 / zoom

    """
    Drawing
    """
    start_frame(screen)

    routing.draw_routing(screen)
    grid.draw_tilecolors(screen)
   
    for s in slice.slices:
        s.draw(screen)
    systems.draw_system_rects(screen, files.locations)
        
    grid.draw_grid(screen) 
    grid.draw_ranges(screen)
    grid.draw_pins(screen)

    systems.draw_system_labels(screen, files.locations)

    connections.draw_connections(screen)
    
    legend.draw_legend(screen, slice.components)
    files.draw_file_indicators(screen)


    # TODO (lowprio): if in een build mapje, allow met pijltjestoetsen te switchen tussen componenten

    pygame.display.flip()
    files.try_load_pnr()
    files.try_load_locs()
