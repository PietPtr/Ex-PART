import json
from pprint import pprint
import pygame
from init import *

class Slice:
    def __init__(self, component_name, x, y, l):
        self.component_name = component_name
        self.x = x
        self.y = y
        self.letter = l

    def draw(self, screen):
        letter_offset = (ord(self.letter) - 65) * SQUARE_SIZE // 4
        

        pygame.draw.rect(screen, color(self.component_name), pygame.Rect(
            self.x * SQUARE_SIZE, 
            self.y * SQUARE_SIZE + letter_offset, 
            SQUARE_SIZE, 
            SQUARE_SIZE // 4))
        pass

slices = []

def parse_nextpnr_bel(belstr):
    res = {}
    components = belstr.split("/")
    res['x'] = int(components[0][1:])
    res['y'] = int(components[1][1:])
    res['letter'] = components[2][-1]
    return res

def build_slices(cells):
    global slices

    for cellname, data in cells.items():
        cmp_name = ".".join(cellname.split(".")[:-1])
        if cmp_name != "":
            belstr = data['attributes']['NEXTPNR_BEL']
            bel = parse_nextpnr_bel(belstr)

            slices.append(Slice(
                cmp_name,
                bel['x'],
                bel['y'],
                bel['letter']
            ))
        else:
            pass # probably not one of my cells