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

screen = pygame.display.set_mode(size)

last_load = 0

bitstream_filename = sys.argv[1] + "/bitstream.json"

def try_load_pnr():
    global pnr
    global last_load
    
    last_mod = os.path.getmtime(bitstream_filename)

    if (last_mod > last_load):
        with open(bitstream_filename) as loc_file:
            try:
                pnr = json.load(loc_file)
                last_load = time.time()
                print("Reloaded", bitstream_filename)
            except json.decoder.JSONDecodeError:
                print("Tried reloading JSON, but found parse errors.")

            try:
                slice.build_slices(pnr["modules"]["top"]["cells"])
            except KeyError:
                print("Bitstream not in correct format.")

try_load_pnr()

while True:
    for event in pygame.event.get():
        handle_event(event)

    """
    Updates and logic
    """
    # mov = pygame.mouse.get_rel()
    # if (pygame.mouse.get_pressed()[0]):
    #     view[0] += mov[0] * 1 / zoom
    #     view[1] += mov[1] * 1 / zoom

    """
    Drawing
    """
    screen.fill((221, 231, 251))

    draw_grid(screen)
    draw_ranges(screen)

    for s in slice.slices:
        s.draw(screen)

    legend.draw_legend(screen, slice.components)

    pygame.display.flip()
    time.sleep(0.1)
    try_load_pnr()
