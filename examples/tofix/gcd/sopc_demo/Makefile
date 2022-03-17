# This Makefile must be invoked from a Nios2 Command Shell (avail in <altera-install-dir>/nios2eds)

SOPCINFO_FILE=./qsys/main.sopcinfo
BSP_OPTS=--set hal.sys_clk_timer none --set hal.timestamp_timer timer 
HW_FILE=./hw/output_files/top.sof

.PHONY: bsp hw qsys quartue

qsys:
	qsys-edit ./qsys/main.qsys

quartus:
	quartus ./hw/top.qpf

bitstream:
	cp ip/*.vhd ./qsys/main/synthesis/submodules
	(cd hw; quartus_map --read_settings_files=on --write_settings_files=off top -c top)
	(cd hw; quartus_fit --read_settings_files=off --write_settings_files=off top -c top)
	(cd hw; quartus_asm --read_settings_files=off --write_settings_files=off top -c top)

bsp:
	nios2-bsp hal ./bsp $(SOPCINFO_FILE) $(BSP_OPTS)

hw: 
	@echo "** Check that the DE10-Lite board is connected and the USB-Blaster connection active"
	nios2-configure-sof $(HW_FILE)

makef:
	(cd sw; nios2-app-generate-makefile --bsp-dir ../bsp --set APP_CFLAGS_OPTIMIZATION -Os --src-files main.c)

build:
	(cd sw; make)

run:
	(cd sw; make download-elf && nios2-terminal)

term:
	nios2-terminal

clean:
	(cd bsp; make clean)
