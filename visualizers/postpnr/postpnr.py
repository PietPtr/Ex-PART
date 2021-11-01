import sys
import pygame
import time
import json
import os
import random
from pprint import pprint
import slice
import init
import legend

screen = pygame.display.set_mode(init.size)

view = [0, 0]
zoom = 1

init.randomize_colors()

x_range = [i for i in range(124)]
y_range = [i for i in range(93)]

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


def draw_ranges(screen):
    global x_range
    global y_range

    x = 0
    for coord in x_range:
        if init.SQUARE_SIZE > 15:
            text = init.myfont.render(str(coord), True, (80, 80, 80))
            screen.blit(text, 
                (x * init.SQUARE_SIZE + init.SQUARE_SIZE - text.get_width(), 0))
            x += 1

    y = 0
    for coord in x_range:
        if init.SQUARE_SIZE > 15:
            text = init.myfont.render(str(coord), True, (80, 80, 80))    
            screen.blit(text, 
                (0, y * init.SQUARE_SIZE + init.SQUARE_SIZE - text.get_height()))
            y += 1

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()
        if event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                init.SQUARE_SIZE = min(int(init.SQUARE_SIZE * 1.1), 500)
            if event.button == 5:
                init.SQUARE_SIZE = max(int(init.SQUARE_SIZE / 1.1), 10)
        if event.type == pygame.KEYDOWN:
            # if event.key == pygame.K_F5:
            #     with open(sys.argv[1]) as loc_file:
            #         locations = [json.load(loc_file)]
            if event.key == pygame.K_F1:
                init.randomize_colors()
        
        if event.type == pygame.VIDEORESIZE:
            init.width = event.w
            init.height = event.h


    """
    Updates and logic
    """
    mov = pygame.mouse.get_rel()
    if (pygame.mouse.get_pressed()[0]):
        view[0] += mov[0] * 1 / zoom
        view[1] += mov[1] * 1 / zoom

    """
    Drawing
    """
    screen.fill((221, 231, 251))

    for x in range(init.width // init.SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (x * init.SQUARE_SIZE * zoom, 0), 
            (x * init.SQUARE_SIZE * zoom, init.height))

    for y in range(init.height // init.SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (0, y * init.SQUARE_SIZE * zoom), 
            (init.width, y * init.SQUARE_SIZE * zoom))


    draw_ranges(screen)

    for s in slice.slices:
        s.draw(screen)

    legend.draw_legend(screen, slice.components)

    pygame.display.flip()
    time.sleep(0.1)
    try_load_pnr()
