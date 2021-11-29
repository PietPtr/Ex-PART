import pygame
from pprint import pprint
import init
import re

pattern = re.compile("[VH]0[1-9][NSWE][0-9]{4}")

pip_counts = {}

def count_pip(pip):
    [from_part, to_part] = pip.split("->")
    from_split = from_part.split("/")
    coords = f"{from_split[0]}/{from_split[1]}"
    if (re.search(pattern, from_split[2])):
        try:
            pip_counts[coords] += 1
        except KeyError:
            pip_counts[coords] = 1


def parse_routing(routing_str):
    for r in routing_str.split(";"):
        if "->" in r:
            pass # It's a PIP
            count_pip(r)
        elif len(r.split("/")) == 3:
            pass # It's a wire ID
            [x, y, wire_name] = r.split("/")

        else:
            pass # It's a strength thingy I guess
            # TODO: check if we're not missing something

def build_routemap(nets):
    for (net, value) in nets.items():
        routing_str = value['attributes']['ROUTING']
        parse_routing(routing_str)

    # pprint(dict(filter(lambda i: i[1] > 48, pip_counts.items())))


def draw_routing(screen):
    pygame.draw.rect(screen, 0xffffff, pygame.Rect(init.view(0, 0), 
       (init.SQUARE_SIZE * init.COLS, init.SQUARE_SIZE * init.ROWS)))

    for (coord, amount) in pip_counts.items():
        (xstr, ystr) = coord.split("/")
        x = int(xstr[1:])
        y = int(ystr[1:])

        MOST_ISH_RESOURCES = 51

        fullness = int((1 - max(0.1, min(amount / MOST_ISH_RESOURCES, 0.8))) * 255)
        route_color = (fullness, fullness, fullness)

        pygame.draw.rect(screen, route_color, pygame.Rect(
            init.view(x * init.SQUARE_SIZE, y * init.SQUARE_SIZE),
            (init.SQUARE_SIZE, init.SQUARE_SIZE)
        ))
