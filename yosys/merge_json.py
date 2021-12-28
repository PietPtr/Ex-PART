import json
import sys

# ISSUE #24: This should not be written in python, but Aeson should be used for this...

with open("interconnect.json") as f:
    new_modules = json.load(f)

with open("base.json") as f:
    old_data = json.load(f)

del old_data["modules"]["\\$top"]

for module in new_modules:
    for (module_name, module_data) in module.items():
        old_data["modules"][module_name] = module_data

with open("synthesized.json", 'w') as f:
    json.dump(old_data, f)