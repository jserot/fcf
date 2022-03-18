FCFC=../../../_build/default/src/bin/fcfc.bc
FCF_LIB=../../../etc/lib/vhdl/fcf.vhd
GHDL=ghdl
GHDLOPTS=-P../../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin
VHDL_OPTS=-vhdl_heap_size 64

run: 
	$(FCFC) -run -trace main.fcf

dot: 
	$(FCFC) -dot -dump_typed main.fcf

show: 
	$(FCFC) -show main.fcf

vhdl: 
	$(FCFC) -vhdl -vhdl_testbench $(VHDL_OPTS) main.fcf

vhdl.run: globals.vhd sum_tree.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) globals.vhd 
	$(GHDL) -a $(GHDLOPTS) sum_tree.vhd 
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
	$(GHDL) -r $(GHDLOPTS) tb --wave=tb.ghw --stop-time=500ns

vhdl.view: tb.vcd
	$(GTKWAVE) -f tb.vcd -a tb.sav> /tmp/gtkwave.log 2>&1

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf
	\rm -f tb tb.vcd

