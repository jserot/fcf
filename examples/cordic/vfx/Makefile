FCFC=../../../_build/default/src/bin/fcfc.bc
GHDL=ghdl
GHDLOPTS=-P../../../etc/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin
FCF_LIB=../../../etc/lib/vhdl/fcf.vhd
QUARTUS_DIR=./quartus
QSYS_DIR=./qsys

run: 
	$(FCFC) -run -trace -array_print_length 1 main.fcf

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

vhdl.quartus: 
	$(FCFC) -vhdl -vhdl_quartus $(QUARTUS_DIR) $(VHDL_OPTS) main.fcf

vhdl.qsys: 
	$(FCFC) -vhdl -vhdl_qsys $(QSYS_DIR) $(VHDL_OPTS) main.fcf

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf
	\rm -f tb tb.vcd
	(cd quartus; make clean)

