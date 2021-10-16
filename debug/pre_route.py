from pprint import pprint
import os


print(f"------------------------ {os.path.basename(__file__)} --------------------------")


for cell, cellinfo in ctx.cells:
    print(dir(cellinfo))
    print(cell, cellinfo)


print(f"------------------------ {os.path.basename(__file__)} --------------------------")

# raise Exception