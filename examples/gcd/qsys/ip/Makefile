all: update

check:
	ghdl -a -fno-color-diagnostics fcf.vhd
	ghdl -a -fno-color-diagnostics gcd.vhd
	ghdl -a -fno-color-diagnostics gcd_cc.vhd

update:
	cp fcf.vhd gcd.vhd gcd_cc.vhd ../qsys/main/synthesis/submodules

clean:
	\rm -f *.o *.cf *~
	\rm *.vhd
