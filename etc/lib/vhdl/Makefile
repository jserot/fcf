GHDL=ghdl
GHDLOPTS=-fno-color-diagnostics

all: lib

lib: fcf.vhd
	$(GHDL) -a --work=fcf --workdir=. $(GHDLOPTS) fcf.vhd

clean:
	\rm -f *.cf *.o

realclean: clean
	\rm -f *~

clobber: realclean
