import json
import sys
import os

top_entity_name = str(ctx.top_module)

def cell_name_to_json_path(cell_name):
    # mooi magic number, hopelijk is het in general ook correct <:]
    expart_name = cell_name.split('.')[:-2] 

    if len(expart_name) == 0:
        return False
    
    assert '_instance_' in expart_name[-1]
    assert all(['_system_' in name for name in expart_name[:-1]])

    json_path = top_entity_name

    while len(expart_name) > 0:
        subname = expart_name.pop(0)
        json_path += '.' + subname.split('_')[-1]

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
            ctx.createRectangularRegion(regionID, 
                sys["tl"]["x"], sys["tl"]["y"],
                sys["br"]["y"], sys["br"]["y"])
            # print("created a region for", regionID)
            # print(f"ctx.createRectangularRegion({regionID}, {sys['tl']['x']}, {sys['tl']['y']}, {sys['br']['x']}, {sys['br']['y']})")
            path.pop()
        else:
            subsystems = sys
            create_regions_system_list(subsystems)


print(f"------------------------ {os.path.basename(__file__)} --------------------------")

# TODO: hoe laden we een arbitraire json in, zonder command line arguments?
with open("../testenv/locations.json") as f:
    locations = json.load(f)

create_regions_system_list([locations])


for cell, cellinfo in ctx.cells:
    if 'BEL' in cellinfo.attrs or 'NEXTPNR_BEL' in cellinfo.attrs:
        pass
    else:
        # TODO: in dit stuk kunnen veel bugs zitten omdat de BEL filtering niet bijzonder specifiek is.
        json_path = cell_name_to_json_path(cell)

        if json_path:
            print(f"constrained cell to {json_path}")
            ctx.constrainCellToRegion(cell, json_path)
        else:
            print(f"WARNING: did not constrain cell {cell} as it did not comply with Ex-PART namings.")



print(f"------------------------ {os.path.basename(__file__)} --------------------------")
