import sys
import pygame
import time
import sys
import json
from pprint import pprint

pygame.init()

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

screen = pygame.display.set_mode(size)

view = [0, 0]
zoom = 1

SQUARE_SIZE = 50


with open(sys.argv[1]) as loc_file:
    locations = [json.load(loc_file)]

def color(name):
    name = name + name + name + name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (n[0] + n[3] << 2) * 1.6,
        (n[1] + n[4] << 2) * 1.6,
        (n[2] + n[5] << 2) * 1.6
    )
    return color

def draw_system(screen, system_list):
    for system in system_list:
        key = next(iter(system))
        if 'br' in system[key] and 'tl' in system[key]:
            left = system[key]['tl']['x'] * SQUARE_SIZE * zoom
            top = system[key]['tl']['y'] * SQUARE_SIZE * zoom
            w = abs(system[key]['tl']['x'] - (system[key]['br']['x'] + 1)) * SQUARE_SIZE * zoom
            h = abs(system[key]['tl']['y'] - (system[key]['br']['y'] + 1)) * SQUARE_SIZE * zoom
            # print(left // SQUARE_SIZE, top // SQUARE_SIZE, w // SQUARE_SIZE, h // SQUARE_SIZE)
            pygame.draw.rect(screen, color(key), pygame.Rect(left, top, w, h))
        else:
            for key in system:
                draw_system(screen, system[key])

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()
        if event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                zoom *= 1.1
            if event.button == 5:
                zoom /= 1.1
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_F5:
                with open(sys.argv[1]) as loc_file:
                    locations = [json.load(loc_file)]

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
