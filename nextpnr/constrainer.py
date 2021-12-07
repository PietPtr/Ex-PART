import json
import sys
import os
from pprint import pprint

top_entity_name = str(ctx.top_module)

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
            # TODO (lowprio): gooi error wanneer een van deze punten out of bounds van de FPGA is
            print(f"ctx.createRectangularRegion('{regionID}', {sys['tl']['x']}, {sys['tl']['y']}, {sys['br']['x']}, {sys['br']['y']})")
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
        # print(f"constrained cell: \n\t{cell} \n\t\tto \n\t{json_path}") 
        ctx.constrainCellToRegion(cell, json_path)
        constrained_ctr += 1
    else:
        # print(f"WARNING: did not constrain cell {cell} as it did not comply with Ex-PART namings.")
        unconstrained_ctr += 1


print(f"Constrained {constrained_ctr} cells, left {unconstrained_ctr} cells unconstrained.")

print(f"------------------------ {os.path.basename(__file__)} --------------------------")
