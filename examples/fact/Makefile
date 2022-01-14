FCFC=../../_build/default/src/bin/fcfc.bc
GHDL=ghdl
GHDLOPTS=-P../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin

run: 
	$(FCFC) -run -trace main.fsm

dot: 
	$(FCFC) -dot main.fsm

show: 
	$(FCFC) -show main.fsm

vhdl: 
	$(FCFC) -vhdl -vhdl_testbench main.fsm

vhdl.check:
	$(GHDL) -a -P../../etc/lib/vhdl -fno-color-diagnostics fact.vhd

vhdl.run: fact.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) fact.vhd 
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
	$(GHDL) -r $(GHDLOPTS) tb --vcd=tb.vcd --stop-time=300ns

vhdl.view: tb.vcd
	$(GTKWAVE) -f tb.vcd -a tb.sav> /tmp/gtkwave.log 2>&1

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf
	\rm -f tb tb.vcd

