import json
from pprint import pprint
import pygame
import color
import init

class Slice:
    def __init__(self, component_name, x, y, l, active):
        self.component_name = component_name
        self.display_name = init.cell_name_to_json_path(component_name).split(".")[-1]
        self.x = x
        self.y = y
        self.letter = l
        self.active = active

    def draw(self, screen):
        letter_offset = (ord(self.letter) - 65) * init.SQUARE_SIZE // 4

        slice_color = pygame.Color(color.color(self.display_name))
        gray = (slice_color.r + slice_color.g + slice_color.b) // 3
        slice_color = slice_color if self.active else (gray, gray, gray)

        pygame.draw.rect(screen, slice_color, pygame.Rect(
            init.view(self.x * init.SQUARE_SIZE,
                 self.y * init.SQUARE_SIZE + letter_offset), 
            (init.SQUARE_SIZE, 
            init.SQUARE_SIZE // 4 + 1)))
        pass

slices = []
components = set()

def parse_nextpnr_bel(belstr):
    res = {}
    components = belstr.split("/")
    res['x'] = int(components[0][1:])
    res['y'] = int(components[1][1:])
    res['letter'] = components[2][-1]
    return res

def build_slices(cells):
    global slices
    global components
    components = set()
    slices = []

    for cellname, data in cells.items():
        cmp_name = ".".join(cellname.split(".")[:-1])
        if cmp_name != "":
            belstr = data['attributes']['NEXTPNR_BEL']
            bel = parse_nextpnr_bel(belstr)

            new_slice = Slice(
                cmp_name,
                bel['x'],
                bel['y'],
                bel['letter'],
                True
            )

            slices.append(new_slice)

            components.add(new_slice.display_name)
        else:
            belstr = data['attributes']['NEXTPNR_BEL']
            try:
                bel = parse_nextpnr_bel(belstr)
            except KeyError:
                print(f"Unknown BEL format {belstr}")
            
            if bel:
                new_slice = Slice(
                    cellname.split('_')[0],
                    bel['x'],
                    bel['y'],
                    bel['letter'],
                    False
                )

                slices.append(new_slice)


    components = sorted(list(components))