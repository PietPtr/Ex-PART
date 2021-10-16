

install:
	mkdir -p /usr/share/ex-part
	mkdir -p /usr/share/ex-part/nextpnr
	mkdir -p /usr/share/ex-part/yosys
	cp yosys/merge_json.py /usr/share/ex-part/yosys/
	cp yosys/grouped.ys /usr/share/ex-part/yosys/
	cp nextpnr/constrainer.py /usr/share/ex-part/nextpnr/