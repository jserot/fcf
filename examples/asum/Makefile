FCFC=../../_build/default/src/bin/fcfc.bc
GHDL=ghdl

run: 
	$(FCFC) -run -trace main.fsm

dot: 
	$(FCFC) -dot -dump_typed main.fsm

show: 
	$(FCFC) -show -dump_typed main.fsm

vhdl: 
	$(FCFC) -vhdl -vhdl_numeric_std main.fsm

vhdl.check:
	$(GHDL) -a -P../../etc/lib/vhdl -fno-color-diagnostics globals.vhd
	$(GHDL) -a -P../../etc/lib/vhdl -fno-color-diagnostics sum.vhd


clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf

