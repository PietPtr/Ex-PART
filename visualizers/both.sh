#!/bin/bash

python3.6 postpnr.py $1 &
python3.6 locations.py $1 &
