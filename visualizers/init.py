
import pygame
import random
import sys

if len(sys.argv) == 1:
    print("Please provide the project folder name.")
    quit()

pygame.init()

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

last_load = 0

pygame.font.init() 
myfont = pygame.font.SysFont('Courier', 14)

SQUARE_SIZE = pygame.display.Info().current_w // 24

def color(name):
    global mods
    name = name + name + name
    n = [ord(n) % 16 for n in name]
    color = (
        (sum(n[::3]) * mods[0]) % 255,
        (sum(n[1::3]) * mods[1]) % 255,
        (sum(n[2::3]) * mods[2]) % 255
    )
    return color

# TODO: color module
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
mods = []

def randomize_colors():
    global mods
    mods = [random.choice(primes), random.choice(primes), random.choice(primes)]

randomize_colors()

def cell_name_to_json_path(cell_name):
    expart_name = cell_name.split('.')

    if len(expart_name) == 0:
        return "NAME ERROR"


    subnames = map(lambda s: s.split('-')[-1], expart_name)
    json_path = ".".join(subnames)

    return json_path


def is_bright(color_any):
    color = pygame.Color(color_any)
    return (color.r * 0.299 + color.g * 0.587 + color.b * 0.114) > 150

def reset_range():
    global x_range, y_range
    x_range = [i for i in range(124)]
    y_range = [i for i in range(93)]

x_range = []
y_range = []
relative_coords = True
reset_range()


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
    for coord in y_range:
        if SQUARE_SIZE > 15:
            text = myfont.render(str(coord), True, (80, 80, 80))    
            screen.blit(text, 
                (0, y * SQUARE_SIZE + SQUARE_SIZE - text.get_height()))
            y += 1

def draw_grid(screen):
    for x in range(width // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (x * SQUARE_SIZE, 0), 
            (x * SQUARE_SIZE, height))

    for y in range(height // SQUARE_SIZE + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            (0, y * SQUARE_SIZE), 
            (width, y * SQUARE_SIZE))

def handle_event(event):
    global SQUARE_SIZE
    global width, x_range
    global height, y_range
    global relative_coords
    if event.type == pygame.QUIT: sys.exit()
    if event.type == pygame.MOUSEBUTTONDOWN:
        if event.button == 4:
            SQUARE_SIZE = min(int(SQUARE_SIZE * 1.1), 500)
        if event.button == 5:
            SQUARE_SIZE = max(int(SQUARE_SIZE / 1.1), 10)
    if event.type == pygame.KEYDOWN:
        if event.key == pygame.K_F1:
            randomize_colors()
        if event.key == pygame.K_r:
            if relative_coords:
                relative_coords = False
                reset_range()
            else:
                relative_coords = True
    if event.type == pygame.VIDEORESIZE:
        width = event.w
        height = event.h
    if event.type == pygame.MOUSEMOTION:
        if relative_coords:
            (x, y) = event.pos
            grid = lambda a : int((a - a % SQUARE_SIZE) / SQUARE_SIZE)
            grid_x = grid(x)
            grid_y = grid(y)
            
            if x_range[grid_x] != 0:
                offset = x_range[grid_x]
                x_range = list(map(lambda x: x - offset, x_range))

            if y_range[grid_y] != 0:
                offset = y_range[grid_y]
                y_range = list(map(lambda y: y - offset, y_range))
            
