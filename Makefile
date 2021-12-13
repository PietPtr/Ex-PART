
NEXTPNR_ECP5_EXEC=/home/pieter/Education/Thesis/stables/nextpnr/nextpnr-ecp5

install:
	mkdir -p /usr/share/ex-part
	mkdir -p /usr/share/ex-part/nextpnr
	mkdir -p /usr/share/ex-part/yosys
	cp yosys/merge_json.py /usr/share/ex-part/yosys/
	cp yosys/grouped.ys /usr/share/ex-part/yosys/
	cp yosys/monolithic.ys /usr/share/ex-part/yosys/
	cp yosys/hierarchic.ys /usr/share/ex-part/yosys/
	cp nextpnr/constrainer.py /usr/share/ex-part/nextpnr/
	ln -s $(NEXTPNR_ECP5_EXEC) /usr/share/ex-part/nextpnr/nextpnr-ecp5
	cp visualizer/tiledata.csv /usr/share/ex-part/visualizer/tiledata.csv
	cp visualizer/iodata.csv /usr/share/ex-part/visualizer/iodata.csv

symlink:
	mkdir -p /usr/share/ex-part
	mkdir -p /usr/share/ex-part/nextpnr
	mkdir -p /usr/share/ex-part/yosys
	mkdir -p /usr/share/ex-part/visualizer
	-ln -s $(shell pwd)/yosys/merge_json.py /usr/share/ex-part/yosys/
	-ln -s $(shell pwd)/yosys/grouped.ys /usr/share/ex-part/yosys/
	-ln -s $(shell pwd)/yosys/monolithic.ys /usr/share/ex-part/yosys/
	-ln -s $(shell pwd)/yosys/hierarchic.ys /usr/share/ex-part/yosys/
	-ln -s $(shell pwd)/nextpnr/constrainer.py /usr/share/ex-part/nextpnr/
	-ln -s $(NEXTPNR_ECP5_EXEC) /usr/share/ex-part/nextpnr/nextpnr-ecp5
	-ln -s $(shell pwd)/visualizer/tiledata.csv /usr/share/ex-part/visualizer/tiledata.csv
	-ln -s $(shell pwd)/visualizer/iodata.csv /usr/share/ex-part/visualizer/iodata.csv
