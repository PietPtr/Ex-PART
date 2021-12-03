import pygame
import init
import slice
from pprint import pprint

# set of two-tuples with source and destination coordinates for I/O connections
conns = set()

OUTPUTS = ['F0', 'F1', 'Q0', 'Q1']


def build_connections(cells):
    print("Building connections...")

    cell_map = {}

    for comp in slice.components:
        cell_map[comp] = {}

    for (cell, content) in cells.items():
        path = init.cell_name_to_json_path(cell).split('.')
        try:
            cmp_name = path[-2]
            cell_map[cmp_name][cell] = content
        except IndexError:
            pass # probably some IO crapje anyway, i.e. not a component

    for (cmp_name, celldir) in cell_map.items():
        connections_for_component(cmp_name, celldir, cells)
        


def setify_lists(lists):
    l = set()
    for list in lists:
        for item in list:
            l.add(item)
    return l

def setify_dict(dict_of_lists):
    l = set()
    for (key, list) in dict_of_lists.items():
        for item in list:
            l.add(item)
    return l


def connections_for_component(cmp_name, cells, all_cells):
    print(f"   ...for component {cmp_name}")
    global conns
    for (cell, content) in cells.items():
        out_nets = setify_lists([content['connections'][o] for o in OUTPUTS])

        for (other_cell, other_content) in all_cells.items():
            # for every cell that is not from this component,
            # check if any of the out_nets is in its connections
            if not (cmp_name in other_cell):
                intsct = (out_nets & setify_dict(other_content['connections']))
                try:
                    other_cmp_name = init.cell_name_to_json_path(other_cell).split('.')[-2]
                except IndexError:
                    if not init.RENDER_COMPONENT_IO_WITH_GLOBAL_IO:
                        continue
                    other_cmp_name = other_cell

                if len(intsct) > 0:
                    conn = (
                        content['attributes']['NEXTPNR_BEL'], # output
                        other_content['attributes']['NEXTPNR_BEL'] # input
                    )

                    conns.add(conn)


def draw_connections(screen):
    global conns

    RADIUS = init.SQUARE_SIZE * 0.8 // 8
    X_OFFSET = RADIUS + init.SQUARE_SIZE // 4
    Y_OFFSET = (init.SQUARE_SIZE // 4) // 2
    
    for conn in conns:
        (from_loc, to_loc) = (conn)
        from_loc = slice.parse_nextpnr_bel(from_loc)
        to_loc = slice.parse_nextpnr_bel(to_loc)

        output = init.view(
            from_loc['x'] * init.SQUARE_SIZE + X_OFFSET,
            from_loc['y'] * init.SQUARE_SIZE + 
                (ord(from_loc['letter']) - 65) * init.SQUARE_SIZE // 4
                 + Y_OFFSET
        )

        input = init.view(
            to_loc['x'] * init.SQUARE_SIZE + X_OFFSET,
            to_loc['y'] * init.SQUARE_SIZE + 
                (ord(to_loc['letter']) - 65) * init.SQUARE_SIZE // 4
                + Y_OFFSET
        )

        light = 0xdddddd
        dark = 0x222222

        if init.RENDER_COMPONENT_IO_WITH_LINES:
            pygame.draw.aaline(screen, dark, output, input)
        
        pygame.draw.circle(screen, light, output, RADIUS)
        pygame.draw.circle(screen, dark, output, RADIUS, 2)
        pygame.draw.circle(screen, dark, input, RADIUS)
        pygame.draw.circle(screen, light, input, RADIUS, 2)

