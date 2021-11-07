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

screen = pygame.display.set_mode(size)

last_load_pnr = 0
last_load_loc = 0
locations = []

bitstream_filename = sys.argv[1] + "/bitstream.json"
locations_filename = sys.argv[1] + "/locations.json"

def try_load_pnr():
    global last_load_pnr
    
    try:
        last_mod = os.path.getmtime(bitstream_filename)
    except FileNotFoundError:
        
        return 

    if (last_mod > last_load_pnr):
        with open(bitstream_filename) as loc_file:
            try:
                pnr = json.load(loc_file)
                last_load_pnr = time.time()
                print("Reloaded", bitstream_filename)
            except json.decoder.JSONDecodeError:
                print("Tried reloading JSON, but found parse errors.")
                return

            try:
                slice.build_slices(pnr["modules"]["top"]["cells"])
            except KeyError:
                print("Bitstream not in correct format.")


def try_load_locs():
    global locations
    global last_load_loc
    
    try:
        last_mod = os.path.getmtime(locations_filename)
    except FileNotFoundError:
        locations = []
        return 

    if (last_mod > last_load_loc):
        with open(locations_filename) as loc_file:
            try:
                locations = [json.load(loc_file)]
                last_load_loc = time.time()
                print("Reloaded", locations_filename)
            except json.decoder.JSONDecodeError:
                print("Tried reloading JSON, but found parse errors.")


try_load_pnr()
try_load_locs()




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

    systems.draw_system_rects(screen, locations)

    for s in slice.slices:
        s.draw(screen)
        
    draw_grid(screen)
    draw_ranges(screen)

    systems.draw_system_labels(screen, locations)
    
    legend.draw_legend(screen, slice.components)

    pygame.display.flip()
    time.sleep(0.1)
    try_load_pnr()
    try_load_locs()
