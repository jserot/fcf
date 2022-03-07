FCFC=../../_build/default/src/bin/fcfc.bc
FCF_LIB=../../etc/lib/vhdl/fcf.vhd
GHDL=ghdl
GHDLOPTS=-P../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin
VHDL_OPTS=

run: 
	$(FCFC) -run -trace main.fcf

dot: 
	$(FCFC) -dot -dump_typed main.fcf

show: 
	$(FCFC) -show main.fcf

vhdl: 
#	$(FCFC) -vhdl -vhdl_testbench $(VHDL_OPTS) main.fcf
#	$(FCFC) -vhdl $(VHDL_OPTS) main.fcf

vhdl.run: values.vhd globals.vhd silly.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) values.vhd
	$(GHDL) -a $(GHDLOPTS) globals.vhd
	$(GHDL) -a $(GHDLOPTS) silly.vhd
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
	$(GHDL) -r $(GHDLOPTS) tb --wave=tb.ghw --stop-time=400ns

vhdl.view: tb.ghw
	$(GTKWAVE) -f tb.ghw -a tb.sav> /tmp/gtkwave.log 2>&1

clean: 
	\rm -f *~
#	\rm -f *.o *.dot *.vhd *.cf
	\rm -f *.o *.dot *.cf
	\rm -f tb tb.vcd
