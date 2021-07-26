FCFC=../../_build/default/src/bin/fcfc.bc
GHDL=ghdl

run: 
	$(FCFC) -run -trace test.fsm

dot: 
	$(FCFC) -dot -dump_typed test.fsm

show: 
	$(FCFC) -show -dump_typed test.fsm

vhdl: 
	$(FCFC) -vhdl -vhdl_numeric_std -vhdl_default_int_size 16 test.fsm

vhdl.check:
	$(GHDL) -a -fno-color-diagnostics fact.vhd


clean: 
	\rm -f *~
	\rm -f *.dot
	\rm -f *.vhdl

