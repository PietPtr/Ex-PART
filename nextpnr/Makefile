
# TODO: maak dit een haskell functie die het uitvoert ipv een makefile

expart: 
	nextpnr-ecp5 --85k --json ../testenv/combined.json \
		--lpf collatz.lpf \
		--textcfg collatzer.config \
		--pre-place constrainer.py \

gui:
	nextpnr-ecp5 --25k --json ../testenv/combined.json \
		--lpf collatz.lpf \
		--gui --gui-no-aa \

