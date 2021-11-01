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

locations_filename = sys.argv[1] + "/locations.json"

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

SQUARE_SIZE = pygame.display.Info().current_w // 24
print(SQUARE_SIZE, pygame.display.Info().current_w)

screen = pygame.display.set_mode(size)
pygame.font.init() 
myfont = pygame.font.SysFont('Courier', 14)

view = [0, 0]
zoom = 1


primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
mods = []
randomize_colors()

x_range = [i for i in range(124)]
y_range = [i for i in range(93)]

with open(locations_filename) as loc_file:
    locations = [json.load(loc_file)]
    last_load = time.time()

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

def color(name):
    name = name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (sum(n[::3]) * mods[0]) % 255,
        (sum(n[1::3]) * mods[1]) % 255,
        (sum(n[2::3]) * mods[2]) % 255
    )
    return color

def is_bright(color_any):
    color = pygame.Color(color_any)
    return (color.r * 0.299 + color.g * 0.587 + color.b * 0.114) > 150

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
            
            rect_color = color(key)
            pygame.draw.rect(screen, rect_color, pygame.Rect(left, top, w, h))

            text_color = (0, 0, 0) if is_bright(rect_color) else (255, 255, 255)
            textsurface = myfont.render(key, True, text_color)
            if h > w:
                textsurface = pygame.transform.rotate(textsurface, -90)

            text_offset = SQUARE_SIZE/10
            if SQUARE_SIZE > 18:
                screen.blit(
                    textsurface,
                    (tl['x'] * SQUARE_SIZE + text_offset, tl['y'] * SQUARE_SIZE + text_offset),
                    area=pygame.Rect(0, 0, w - 2*text_offset, h - 2*text_offset))
        
        else:
            for key in system:
                draw_system(screen, system[key])

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
                with open(locations_filename) as loc_file:
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
    screen.fill((221, 231, 251))

    for x in range(width // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (x * SQUARE_SIZE * zoom, 0), 
            (x * SQUARE_SIZE * zoom, height))

    for y in range(height // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (0, y * SQUARE_SIZE * zoom), 
            (width, y * SQUARE_SIZE * zoom))

    draw_system(screen, locations)
    draw_ranges(screen)


    pygame.display.flip()
    time.sleep(0.1)
    try_load_locs()
