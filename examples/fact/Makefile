FCFC=../../_build/default/src/bin/fcfc.bc

run: 
	$(FCFC) -run -trace test.fsm

dot: 
	$(FCFC) -dot test.fsm

show: 
	$(FCFC) -show test.fsm

vhdl: 
	$(FCFC) -vhdl test.fsm

clean: 
	\rm -f *~
	\rm -f *.dot
	\rm -f *.vhdl

