
GHDL=ghdl
GHDLOPTS=-P../../../etc/lib/vhdl -fno-color-diagnostics

.PHONY: quartus

quartus:
	quartus ./cordic.qpf

check:
	$(GHDL) -a $(GHDLOPTS) utils.vhd
	$(GHDL) -a $(GHDLOPTS) globals.vhd
	$(GHDL) -a $(GHDLOPTS) cordic.vhd

bitstream:
# This rule must be invoked from a Nios2 Command Shell (avail in <altera-install-dir>/nios2eds)
	(quartus_map --read_settings_files=on --write_settings_files=off cordic -c cordic)
	(quartus_fit --read_settings_files=off --write_settings_files=off cordic -c cordic)
	(quartus_asm --read_settings_files=off --write_settings_files=off cordic -c cordic)

clean:
	\rm -f *.o *.cf *~
	\rm -f globals.vhd cordic.vhd
	\rm -f cordic.qpf cordic.qsf
