
import pygame
import sys
import color
import argparse

# if len(sys.argv) == 1:
#     print("Please provide the project folder name.")
#     quit()

parser = argparse.ArgumentParser()
parser.add_argument("dir", help="An Ex-PART output directory to visualize.")
parser.add_argument("-c", "--connections", 
    help="Show connections (may take a long time at start-up)", 
    action="store_true")
args = parser.parse_args()

pygame.base.init()
pygame.font.init()

size = width, height = 1280, 720
black = 0, 0, 0
white = 255, 255, 255
red = 255, 100, 100

last_load = 0

pygame.font.init() 
myfont = pygame.font.SysFont('Courier', 14)

BASE_SS = pygame.display.Info().current_w // 75
SQUARE_SIZE = BASE_SS
VIEW = [0, 0]
ROWS = 96
COLS = 127
RENDER_COMPONENT_IO = args.connections
RENDER_COMPONENT_IO_WITH_LINES = True
RENDER_COMPONENT_IO_WITH_GLOBAL_IO = False
MOUSE_TILE = (0, 0)
OVER_COMPONENT = None


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

    # set blank spots for rows where the ECP5 85k does not have LUTs:
    # ECP5_85K_NO_LUT_ROWS = [10, 22, 34, 46, 58, 70, 82]
    # for row in ECP5_85K_NO_LUT_ROWS:
    #     y_range.insert(row, " ")

x_range = []
y_range = []
relative_coords = False
reset_range()

def view(x, y):
    global VIEW
    return (x + VIEW[0], y + VIEW[1])


def handle_event(event):
    global SQUARE_SIZE
    global MOUSE_TILE
    global width, x_range
    global height, y_range
    global relative_coords
    if event.type == pygame.QUIT: sys.exit()
    if event.type == pygame.MOUSEBUTTONDOWN:
        if event.button == 4:
            SS_MAX = 500
            SQUARE_SIZE = min(round(SQUARE_SIZE * 1.1), SS_MAX)
            if SQUARE_SIZE != SS_MAX:
                VIEW[0] *= 1.1
                VIEW[1] *= 1.1
        if event.button == 5:
            SS_MIN = 7
            SQUARE_SIZE = max(round(SQUARE_SIZE / 1.1), SS_MIN)
            if SQUARE_SIZE != SS_MIN:
                VIEW[0] /= 1.1
                VIEW[1] /= 1.1
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
        (x, y) = event.pos
        vx = max(0, x - VIEW[0])
        vy = max(0, y - VIEW[1])
        vx = int(vx / SQUARE_SIZE)
        vy = int(vy / SQUARE_SIZE)
        MOUSE_TILE = (vx, vy)

        if relative_coords:
            grid = lambda a : int((a - a % SQUARE_SIZE) / SQUARE_SIZE)
            grid_x = grid(x)
            grid_y = grid(y)
            
            if x_range[grid_x] != 0:
                offset = x_range[grid_x]
                x_range = list(map(lambda x: x - offset, x_range))

            if y_range[grid_y] != 0:
                offset = y_range[grid_y]
                y_range = list(map(lambda y: y - offset, y_range))
            
def start_frame(screen):
    global OVER_COMPONENT
    screen.fill((221, 231, 251))
    
    OVER_COMPONENT = None