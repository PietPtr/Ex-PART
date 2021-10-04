import json
import sys

# TODO: het is zo stom dat dit in python is, waarom kan dit niet even nice ook in Aeson :(

with open(sys.argv[1] + "/interconnect.json") as f:
    new_modules = json.load(f)

with open(sys.argv[1] + "/synthesized.json") as f:
    old_data = json.load(f)

del old_data["modules"]["$top"]

for module in new_modules:
    # always just one but whatever
    for (module_name, module_data) in module.items():
        old_data["modules"][module_name] = module_data

with open(sys.argv[1] + "/combined.json", 'w') as f:
    json.dump(old_data, f)