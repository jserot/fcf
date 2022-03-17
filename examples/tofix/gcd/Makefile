FCFC=../../_build/default/src/bin/fcfc.bc
FCF_LIB=../../etc/lib/vhdl/fcf.vhd
GHDL=ghdl
GHDLOPTS=-P../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin
SOPC_DIR=./sopc_demo
VHDL_OPTS=

run: 
	$(FCFC) -run -trace main.fcf

dot: dot.code dot.view

dot.code: 
	$(FCFC) -dot main.fcf

dot.show: 
	$(FCFC) -show main.fcf

vhdl: vhdl.code vhdl.run

vhdl.code: 
	$(FCFC) -dump_typed -vhdl -vhdl_testbench $(VHDL_OPTS) main.fcf

vhdl.run: gcd.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) gcd.vhd 
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
	$(GHDL) -r $(GHDLOPTS) tb --vcd=tb.vcd --stop-time=600ns

vhdl.view: tb.vcd
	$(GTKWAVE) -f tb.vcd -a tb.sav> /tmp/gtkwave.log 2>&1

sopc: 
	$(FCFC) -vhdl -vhdl_sopc $(SOPC_DIR) $(VHDL_OPTS) main.fcf
	cp $(FCF_LIB) $(SOPC_DIR)/ip

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf
	\rm -f tb tb.vcd

