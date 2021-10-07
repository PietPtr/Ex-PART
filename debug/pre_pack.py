from pprint import pprint
import os


print(f"------------------------ {os.path.basename(__file__)} --------------------------")



for cell, cellinfo in ctx.cells:
    print(cell)


print(f"------------------------ {os.path.basename(__file__)} --------------------------")

# raise Exception