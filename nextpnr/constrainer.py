import json
import sys
import os
from pprint import pprint

top_entity_name = str(ctx.top_module)

ECP5_ROWS = 95
ECP5_COLS = 126

def cell_name_to_json_path(cell_name):
    full_name = cell_name.split('.')
    expart_name = full_name[:-1]

    if len(expart_name) == 0:
        return False

    
    assert '-instance-' in expart_name[-1]
    assert all(['-system-' in name for name in expart_name[:-1]])

    json_path = top_entity_name

    while len(expart_name) > 0:
        subname = expart_name.pop(0)
        json_path += '.' + subname.split('-')[-1]

    return json_path

path = []

def create_regions_system_list(system_list):
    for system in system_list:
        for key in system:
            path.append(key)
            create_regions_system(system)

def create_regions_system(system):
    for key in system:
        sys = system[key]
        if "tl" in sys and "br" in sys:
            # this is a bottom mealy
            regionID = ".".join(path)

            print(f"ctx.createRectangularRegion('{regionID}', {sys['tl']['x']}, {sys['tl']['y']}, {sys['br']['x']}, {sys['br']['y']})")
            if (sys["tl"]["x"] > ECP5_COLS or sys["tl"]["x"] < 0 or
                sys["tl"]["y"] > ECP5_ROWS or sys["tl"]["y"] < 0 or
                sys["br"]["x"] > ECP5_COLS or sys["br"]["x"] < 0 or
                sys["br"]["y"] > ECP5_ROWS or sys["br"]["y"] < 0):
                raise Exception("Point out of bounds!")

            ctx.createRectangularRegion(regionID, 
                int(sys["tl"]["x"]), int(sys["tl"]["y"]),
                int(sys["br"]["x"]), int(sys["br"]["y"]))
            path.pop()
        else:
            subsystems = sys
            create_regions_system_list(subsystems)
            path.pop()


print(f"------------------------ {os.path.basename(__file__)} --------------------------")

with open("locations.json") as f:
    locations = json.load(f)

create_regions_system_list([locations])

print(f"Analyzing {len(ctx.cells)} cells.")


constrained_ctr = 0
unconstrained_ctr = 0
for cell, cellinfo in ctx.cells:
    json_path = cell_name_to_json_path(cell)

    if json_path:
        ctx.constrainCellToRegion(cell, json_path)
        constrained_ctr += 1
    else:
        unconstrained_ctr += 1


print(f"Constrained {constrained_ctr} cells, left {unconstrained_ctr} cells unconstrained.")

print(f"------------------------ {os.path.basename(__file__)} --------------------------")
