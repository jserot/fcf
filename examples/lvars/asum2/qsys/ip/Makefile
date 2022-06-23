all: check

check:
	ghdl -a -fno-color-diagnostics utils.vhd
	ghdl -a -fno-color-diagnostics globals.vhd
	ghdl -a -fno-color-diagnostics sum.vhd
	ghdl -a -fno-color-diagnostics sum_cc.vhd

clean:
	\rm -f *.o *.cf *~
	\rm *.vhd
