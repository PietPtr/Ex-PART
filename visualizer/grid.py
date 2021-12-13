import pygame
import init
import csv
from color import *

with open('/usr/share/ex-part/visualizer/tiledata.csv', 'r') as file:
    reader = csv.reader(file)
    tiledata = list(reader)

with open('/usr/share/ex-part/visualizer/iodata.csv', 'r') as file:
    reader = csv.reader(file)
    iodata = list(reader)


def parse_grid_coord(coord):
    split = coord.split("C")
    y = int(split[0][1:])
    x = int(split[1])
    return (x, y)

def draw_tilecolors(screen):
    OFFSET = init.SQUARE_SIZE * 0.2
    for [name, coord, tile_color_str] in tiledata:
        (x, y) = parse_grid_coord(coord)
        tile_color = contrast(pygame.Color(tile_color_str))
        if is_grey(tile_color):
            tile_color = invert(tile_color)
        pygame.draw.rect(screen, tile_color, pygame.Rect(
                init.view(x * init.SQUARE_SIZE + OFFSET, y * init.SQUARE_SIZE + OFFSET), 
                (init.SQUARE_SIZE - OFFSET * 2, init.SQUARE_SIZE - OFFSET * 2)
            ), width=2)


def draw_ranges(screen):
    global x_range
    global y_range

    if init.SQUARE_SIZE > 15:
        W = 14
        (vx, vy) = init.view(-W, -W)
        pygame.draw.rect(screen, 0xffffff, pygame.Rect(max(0, vx), max(0, vy), init.SQUARE_SIZE * init.COLS, W))
        pygame.draw.rect(screen, 0xffffff, pygame.Rect(max(0, vx), max(0, vy), W, init.SQUARE_SIZE * init.COLS))


        x = 0
        for coord in init.x_range:
                text = init.myfont.render(str(coord), True, (80, 80, 80))
                (vx, vy) = init.view(x * init.SQUARE_SIZE + init.SQUARE_SIZE - text.get_width(), -W)
                screen.blit(text, (vx, max(vy, 0)))
                x += 1


        y = 0
        for coord in init.y_range:
                text = init.myfont.render(str(coord), True, (80, 80, 80))    
                (vx, vy) = init.view(-W, y * init.SQUARE_SIZE + init.SQUARE_SIZE - text.get_height())
                screen.blit(text, (max(vx, 0), vy))
                    
                y += 1

def draw_grid(screen):
    for x in range(init.COLS + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            init.view(x * init.SQUARE_SIZE, 0), 
            init.view(x * init.SQUARE_SIZE, init.ROWS * init.SQUARE_SIZE))

    for y in range(init.ROWS + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            init.view(0, y * init.SQUARE_SIZE), 
            init.view(init.COLS * init.SQUARE_SIZE, y * init.SQUARE_SIZE))