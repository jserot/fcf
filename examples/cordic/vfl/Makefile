FCFC=../../../_build/default/src/bin/fcfc.bc
GHDL=ghdl
GHDLOPTS=-P../../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin

run: 
	$(FCFC) -run  main.fcf

dot: dot.code dot.view

dot.code: 
	$(FCFC) -dot main.fcf

dot.view: 
	$(FCFC) -show main.fcf

vhdl: vhdl.code vhdl.run

vhdl.code: 
	$(FCFC) -vhdl -vhdl_testbench main.fcf

vhdl.check:
	$(GHDL) -a $(GHDLOPTS) globals.vhd
	$(GHDL) -a $(GHDLOPTS) cordic.vhd
	$(GHDL) -a $(GHDLOPTS) tb.vhd

vhdl.run: globals.vhd cordic.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) globals.vhd 
	$(GHDL) -a $(GHDLOPTS) cordic.vhd 
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
	$(GHDL) -r $(GHDLOPTS) tb --vcd=tb.vcd --stop-time=800ns

vhdl.view: tb.vcd
	$(GTKWAVE) -f tb.vcd -a tb.sav> /tmp/gtkwave.log 2>&1

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf
	\rm -f tb tb.vcd

