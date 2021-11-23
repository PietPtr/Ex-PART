#!/bin/bash

python3 postpnr.py $1 &
python3 locations.py $1 &
