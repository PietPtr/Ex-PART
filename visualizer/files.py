import slice
import init
import pygame
import sys
import json
import time
import os
import routing
import connections

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
        last_load_pnr = 0
        slice.components = set()
        slice.slices = []
        routing.pip_counts = {}
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
                routing.build_routemap(pnr["modules"]["top"]["netnames"])
                if init.RENDER_COMPONENT_IO:
                    connections.build_connections(pnr["modules"]["top"]["cells"])
            except KeyError:
                print("Bitstream not in correct format.")



def try_load_locs():
    global locations
    global last_load_loc
    
    try:
        last_mod = os.path.getmtime(locations_filename)
    except FileNotFoundError:
        locations = []
        last_load_loc = 0
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

def file_color(load_time):
    if load_time == 0:
        return (217, 88, 74)
    else:
        return (84, 199, 116)

def draw_file_indicators(screen):
    pnr_text = init.myfont.render(bitstream_filename, True, (80, 80, 80))
    loc_text = init.myfont.render(locations_filename, True, (80, 80, 80))

    r = pnr_text.get_height()

    h = init.height - pnr_text.get_height() - 2
    screen.blit(pnr_text, (init.width - pnr_text.get_width() - 2 - r, h))

    pygame.draw.rect(screen, file_color(last_load_pnr), pygame.Rect(
        init.width - r, h, r, r
    ))

    h -= loc_text.get_height() - 2
    screen.blit(loc_text, (init.width - loc_text.get_width() - 2 - r, h))

    pygame.draw.rect(screen, file_color(last_load_loc), pygame.Rect(
        init.width - r, h, r, r
    ))