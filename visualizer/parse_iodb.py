# Generates a file with names of I/O pins at locations from an iodb.json 
# (found in /usr/share/trellis/database/ECP5/ etc), and an lpf file coupling
# names to sites. Usually it's best to use an example lpf, like blinky.lpf
# that is included here, such that the visualiser displays the actual devices
# to which the IO pins link

import json
import csv
from pprint import pprint

PACKAGE = "CABGA381"

iodb = json.load(open('iodb.json'))

site_comps = {}

with open('blinky.lpf') as file:
    lpf = file.read().split("\n")
    for line in lpf:
        if line.startswith("LOCATE"):
            site = line.split("SITE ")[1].split(";")[0].replace('"', '')
            comp = line.split("COMP ")[1].split(" ")[0].replace('"', '')
            site_comps[site] = comp

out = open('iodata.csv', 'w')
writer = csv.writer(out)

for (site, siteinfo) in iodb['packages'][PACKAGE].items():
    if site in site_comps:
        print(site, site_comps[site], siteinfo)
        writer.writerow([site, site_comps[site], siteinfo['col'], siteinfo['row']])

out.close()