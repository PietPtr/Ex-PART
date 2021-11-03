import sys
import pygame
import time
import json
import os
import random
import init
from init import *
from pprint import pprint

locations_filename = sys.argv[1] + "/locations.json"

screen = pygame.display.set_mode(size)


def try_load_locs():
    global locations
    global last_load
    
    last_mod = os.path.getmtime(locations_filename)

    if (last_mod > last_load):
        with open(locations_filename) as loc_file:
            try:
                locations = [json.load(loc_file)]
                last_load = time.time()
                print("Reloaded", locations_filename)
            except json.decoder.JSONDecodeError:
                print("Tried reloading JSON, but found parse errors.")

try_load_locs()

def draw_system(screen, system_list):
    for system in system_list:
        key = next(iter(system))
        if 'br' in system[key] and 'tl' in system[key]:
            tl = system[key]['tl']
            br = system[key]['br']
            left = tl['x'] * init.SQUARE_SIZE
            top = tl['y'] * init.SQUARE_SIZE 
            w = abs(tl['x'] - (br['x'] + 1)) * init.SQUARE_SIZE 
            h = abs(tl['y'] - (br['y'] + 1)) * init.SQUARE_SIZE 
            
            rect_color = color(key)
            pygame.draw.rect(screen, rect_color, pygame.Rect(left, top, w, h))

            text_color = (0, 0, 0) if is_bright(rect_color) else (255, 255, 255)
            textsurface = myfont.render(key, True, text_color)
            if h > w:
                textsurface = pygame.transform.rotate(textsurface, -90)

            text_offset = init.SQUARE_SIZE/10
            if init.SQUARE_SIZE > 18:
                screen.blit(
                    textsurface,
                    (tl['x'] * init.SQUARE_SIZE + text_offset, tl['y'] * init.SQUARE_SIZE + text_offset),
                    area=pygame.Rect(0, 0, w - 2*text_offset, h - 2*text_offset))
        
        else:
            for key in system:
                draw_system(screen, system[key])



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
    draw_system(screen, locations)
    draw_ranges(screen)


    pygame.display.flip()
    time.sleep(0.1)
    try_load_locs()
