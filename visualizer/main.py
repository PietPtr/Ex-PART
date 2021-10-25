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

SQUARE_SIZE = 75

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
mods = []
randomize_colors()

with open(sys.argv[1]) as loc_file:
    locations = [json.load(loc_file)]
    last_load = time.time()
    print(last_load)

def try_load_locs():
    global locations
    global last_load
    
    last_mod = os.path.getmtime(sys.argv[1])

    if (last_mod > last_load):
        with open(sys.argv[1]) as loc_file:
            try:
                locations = [json.load(loc_file)]
                last_load = time.time()
                print("Reloaded", sys.argv[1])
            except json.decoder.JSONDecodeError:
                print("Tried reloading JSON, but found parse errors.")

def color(name):
    name = name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (sum(n[::3]) * mods[0]) % 255,
        (sum(n[1::3]) * mods[1]) % 255,
        (sum(n[2::3]) * mods[2]) % 255
    )
    return color

def draw_system(screen, system_list):
    for system in system_list:
        key = next(iter(system))
        if 'br' in system[key] and 'tl' in system[key]:
            tl = system[key]['tl']
            br = system[key]['br']
            left = tl['x'] * SQUARE_SIZE * zoom
            top = tl['y'] * SQUARE_SIZE * zoom
            w = abs(tl['x'] - (br['x'] + 1)) * SQUARE_SIZE * zoom
            h = abs(tl['y'] - (br['y'] + 1)) * SQUARE_SIZE * zoom
            
            pygame.draw.rect(screen, color(key), pygame.Rect(left, top, w, h))
            textsurface = myfont.render(key, True, (0, 0, 0))
            if h > w:
                textsurface = pygame.transform.rotate(textsurface, -90)
            screen.blit(textsurface,(tl['x'] * SQUARE_SIZE + SQUARE_SIZE/10, tl['y'] * SQUARE_SIZE + SQUARE_SIZE / 10))
        else:
            for key in system:
                draw_system(screen, system[key])

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()
        # if event.type == pygame.MOUSEBUTTONDOWN:
        #     if event.button == 4:
        #         zoom *= 1.1
        #     if event.button == 5:
        #         zoom /= 1.1
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
        pygame.draw.line(screen, 0xaaaaaa, (x * SQUARE_SIZE * zoom, 0), (x * SQUARE_SIZE * zoom, height))

    for y in range(height // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, (0, y * SQUARE_SIZE * zoom), (width, y * SQUARE_SIZE * zoom))


    draw_system(screen, locations)

    pygame.display.flip()
    time.sleep(0.1)
    try_load_locs()
