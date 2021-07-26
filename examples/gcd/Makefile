FCFC=../../_build/default/src/bin/fcfc.bc
GHDL=ghdl

run: 
	$(FCFC) -run -trace test.fsm

dot: 
	$(FCFC) -dot -dump_typed test.fsm

show: 
	$(FCFC) -show -dump_typed test.fsm

vhdl: 
	$(FCFC) -vhdl -vhdl_numeric_std test.fsm

vhdl.check:
	$(GHDL) -a -P../../etc/lib/vhdl -fno-color-diagnostics gcd.vhd


clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf

