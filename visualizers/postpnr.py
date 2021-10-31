import sys
import pygame
import time
import json
import os
import random
from pprint import pprint

def randomize_colors():
    global mods
    mods = [random.choice(primes), random.choice(primes), random.choice(primes)]

pygame.init()

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

screen = pygame.display.set_mode(size)
pygame.font.init() 
myfont = pygame.font.SysFont('Courier', 14)

view = [0, 0]
zoom = 1

SQUARE_SIZE = pygame.display.Info().current_w // 25

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
mods = []
randomize_colors()

x_range = [i for i in range(124)]
y_range = [i for i in range(93)]

# with open(sys.argv[1]) as loc_file:
#     locations = [json.load(loc_file)]
#     last_load = time.time()

# def try_load_locs():
#     global locations
#     global last_load
    
#     last_mod = os.path.getmtime(sys.argv[1])

#     if (last_mod > last_load):
#         with open(sys.argv[1]) as loc_file:
#             try:
#                 locations = [json.load(loc_file)]
#                 last_load = time.time()
#                 print("Reloaded", sys.argv[1])
#             except json.decoder.JSONDecodeError:
#                 print("Tried reloading JSON, but found parse errors.")

def color(name):
    name = name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (sum(n[::3]) * mods[0]) % 255,
        (sum(n[1::3]) * mods[1]) % 255,
        (sum(n[2::3]) * mods[2]) % 255
    )
    return color


def draw_ranges(screen):
    global x_range
    global y_range

    x = 0
    for coord in x_range:
        if SQUARE_SIZE > 15:
            text = myfont.render(str(coord), True, (80, 80, 80))
            screen.blit(text, 
                (x * SQUARE_SIZE + SQUARE_SIZE - text.get_width(), 0))
            x += 1

    y = 0
    for coord in x_range:
        if SQUARE_SIZE > 15:
            text = myfont.render(str(coord), True, (80, 80, 80))    
            screen.blit(text, 
                (0, y * SQUARE_SIZE + SQUARE_SIZE - text.get_height()))
            y += 1

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()
        if event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                SQUARE_SIZE = min(int(SQUARE_SIZE * 1.1), 500)
            if event.button == 5:
                SQUARE_SIZE = max(int(SQUARE_SIZE / 1.1), 10)
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_F5:
                with open(sys.argv[1]) as loc_file:
                    locations = [json.load(loc_file)]
            if event.key == pygame.K_F1:
                randomize_colors()

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
    screen.fill((201, 211, 221))

    for x in range(width // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (x * SQUARE_SIZE * zoom, 0), 
            (x * SQUARE_SIZE * zoom, height))

    for y in range(height // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (0, y * SQUARE_SIZE * zoom), 
            (width, y * SQUARE_SIZE * zoom))


    draw_ranges(screen)


    pygame.display.flip()
    time.sleep(0.1)
    # try_load_locs()
