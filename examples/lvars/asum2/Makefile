FCFC=../../../_build/default/src/bin/fcfc.bc
GHDL=ghdl
GHDLOPTS=-P../../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin

run: 
	$(FCFC) -run -trace main.fcf

dot: dot.code dot.view

dot.code: 
	$(FCFC) -dump_typed -dot main.fcf

dot.view: 
	$(FCFC) -show main.fcf

vhdl: vhdl.code vhdl.run

vhdl.code: 
	$(FCFC) -vhdl -vhdl_testbench $(VHDL_OPTS) main.fcf

vhdl.quartus: 
	$(FCFC) -vhdl -vhdl_quartus /Users/jserot/Desktop/SF2/vhdl/fcf/asum2 main.fcf

vhdl.check:
	$(GHDL) -a -P../../../etc/lib/vhdl -fno-color-diagnostics globals.vhd
	$(GHDL) -a -P../../../etc/lib/vhdl -fno-color-diagnostics sum.vhd
	$(GHDL) -a -P../../../etc/lib/vhdl -fno-color-diagnostics tb.vhd

vhdl.run: globals.vhd sum.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) globals.vhd 
	$(GHDL) -a $(GHDLOPTS) sum.vhd 
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
	$(GHDL) -r $(GHDLOPTS) tb --vcd=tb.vcd --stop-time=400ns

vhdl.view: tb.vcd
	$(GTKWAVE) -f tb.vcd -a tb.sav> /tmp/gtkwave.log 2>&1

vhdl.qsys: 
	$(FCFC) -vhdl -vhdl_sopc $(SOPC_DIR) $(VHDL_OPTS) main.fcf
	cp $(FCF_LIB) $(SOPC_DIR)/ip

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf
	\rm -f tb tb.vcd

