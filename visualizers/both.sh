#!/bin/bash

python postpnr.py $1 &
python locations.py $1 &