import pygame
import init
import csv
from color import *
from pprint import pprint

class PinTag():
    loc_ctr = dict()

    def __init__(self, site, pin_name, x_str, y_str):
        self.x = int(x_str)
        self.y = int(y_str)
        
        if self.x == 0 or self.y == 0:
            self.text = f"{pin_name} {site}"
        else:
            self.text = f"{site} {pin_name}"
            

    def draw(self, screen):
        if init.SQUARE_SIZE < init.BASE_SS:
            return
        zoom = (init.SQUARE_SIZE / init.BASE_SS) * 0.24
        GRID_DIST = 150 * zoom

        location = (self.x, self.y)
        if location not in PinTag.loc_ctr:
            PinTag.loc_ctr[location] = 0
        text = init.largefont.render(self.text, True, (80, 80, 80))
        text_width, text_height = init.largefont.size(self.text)
        text = pygame.transform.scale(text, (int(text_width * zoom), int(text_height * zoom)))
        text_width *= zoom
        text_height *= zoom

        x_offset = 0
        y_offset = 0
        line_x = 0
        line_y = 0

        (x, y) = (
            self.x * init.SQUARE_SIZE,
            self.y * init.SQUARE_SIZE
        )

        loc_count = PinTag.loc_ctr[location]

        if self.x == 0:
            x_offset = -text_width - GRID_DIST
            y_offset = loc_count * text_height
            line_x = x_offset + text_width
            line_y = loc_count * text_height + text_height / 2
        elif self.x == 126:
            x_offset = init.SQUARE_SIZE + GRID_DIST
            y_offset = loc_count * text_height
            line_x = x_offset
            line_y = y_offset + text_height / 2
        elif self.y == 0:
            text = pygame.transform.rotate(text, 270)
            y_offset = -text_width - GRID_DIST
            x_offset = loc_count * text_height
            line_x = x_offset + text_height / 2
            line_y = y_offset + text_width
        elif self.y == 95:
            text = pygame.transform.rotate(text, 270)
            y_offset = GRID_DIST
            x_offset = loc_count * text_height
            line_x = x_offset + text_height / 2
            line_y = y_offset


        (vx, vy) = init.view(x + x_offset, y + y_offset)
        
        screen.blit(text, (vx, vy))
        PinTag.loc_ctr[location] += 1

        pygame.draw.line(screen, (80, 80, 80),
            init.view(x + line_x, y + line_y),
            init.view(self.x * init.SQUARE_SIZE + init.SQUARE_SIZE / 2, self.y * init.SQUARE_SIZE + init.SQUARE_SIZE / 2)
        )





tags = []

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
    if init.PAPER:
        return

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

def make_io():
    for [site_name, pin_name, x, y] in iodata:
        tag = PinTag(
            site_name,
            pin_name,
            x,
            y
        )
        tags.append(tag)


make_io()

def draw_pins(screen):
    global tags

    if init.PAPER:
        return
    
    PinTag.loc_ctr = dict()
    for pin in tags:
        pin.draw(screen)

def draw_ranges(screen):
    global x_range
    global y_range
    if init.PAPER:
        return

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
    rows = init.ROWS
    if init.PAPER:
        rows -= len(init.ECP5_85K_NO_LUT_ROWS)

    for x in range(init.COLS + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            init.view(x * init.SQUARE_SIZE, 0), 
            init.view(x * init.SQUARE_SIZE, rows * init.SQUARE_SIZE))

    for y in range(rows + 1):
        pygame.draw.line(screen, 0xaaaaaa, 
            init.view(0, y * init.SQUARE_SIZE), 
            init.view(init.COLS * init.SQUARE_SIZE, y * init.SQUARE_SIZE))