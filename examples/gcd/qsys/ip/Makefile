all: check

check:
#	ghdl -a -fno-color-diagnostics globals.vhd
	ghdl -a -fno-color-diagnostics utils.vhd
	ghdl -a -fno-color-diagnostics gcd.vhd
	ghdl -a -fno-color-diagnostics gcd_cc.vhd

clean:
	\rm -f *.o *.cf *~
	\rm -f globals.vhd gcd.cc gcd_cc.vhd
