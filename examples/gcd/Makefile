FCFC=../../_build/default/src/bin/fcfc.bc
FCF_LIB=../../etc/lib/vhdl/fcf.vhd
SOPC_DIR=./sopc_demo
VHDL_OPTS=-vhdl_numeric_std
GHDL=ghdl

run: 
	$(FCFC) -run -trace main.fsm

dot: 
	$(FCFC) -dot -dump_typed main.fsm

show: 
	$(FCFC) -show -dump_typed main.fsm

vhdl: 
	$(FCFC) -vhdl $(VHDL_OPTS) main.fsm

sopc: 
	$(FCFC) -vhdl -sopc $(SOPC_DIR) $(VHDL_OPTS) main.fsm
	cp $(FCF_LIB) $(SOPC_DIR)/ip

clean: 
	\rm -f *~
	\rm -f *.o *.dot *.vhd *.cf

