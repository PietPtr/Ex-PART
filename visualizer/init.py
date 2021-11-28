
import pygame
import sys
import color

if len(sys.argv) == 1:
    print("Please provide the project folder name.")
    quit()


pygame.base.init()
pygame.font.init()

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

last_load = 0

pygame.font.init() 
myfont = pygame.font.SysFont('Courier', 14)

BASE_SS = pygame.display.Info().current_w // 24
SQUARE_SIZE = BASE_SS
VIEW = [0, 0]
ROWS = 96
COLS = 127


def cell_name_to_json_path(cell_name):
    expart_name = cell_name.split('.')

    if len(expart_name) == 0:
        return "NAME ERROR"


    subnames = map(lambda s: s.split('-')[-1], expart_name)
    json_path = ".".join(subnames)

    return json_path


def reset_range():
    global x_range, y_range
    x_range = [i for i in range(124)]
    y_range = [i for i in range(93)]

x_range = []
y_range = []
relative_coords = False
reset_range()

def view(x, y):
    global VIEW
    return (x + VIEW[0], y + VIEW[1])


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
            color.randomize_colors()
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
            
