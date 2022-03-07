GHDL=ghdl
GHDLOPTS=-fno-color-diagnostics

all: lib

lib: utils.vhd values.vhd
	$(GHDL) -a --work=fcf --workdir=. $(GHDLOPTS) utils.vhd
	$(GHDL) -a --work=fcf --workdir=. $(GHDLOPTS) values.vhd

clean:
	\rm -f *.cf *.o

realclean: clean
	\rm -f *~

clobber: realclean
