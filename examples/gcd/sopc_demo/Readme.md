This directory contains all the files required to use the VHDL code generated by FCFC for the GCD example 
as a custom component integrated into a Nio2-based SOPC.

The system is made of 
- a Nios2 soft-core CPU 
- 128k on-chip memory
- a JTAG-based UART for displaying results on a Nios2 terminal running on the host PC
- a timer (for measuring computation times)
- the GCD core IP (generated by fcfc) wrapped as a custom component to be accessed as a Avalon MM slave

Running this example requires
- a DE10-Lite FPGA board
- Quartus 15.1 IDE from Intel (to be downloaded from [Intel FPGA website](https://fpgasoftware.intel.com)). 

Instructions

1. From the GCD example top directory (./..), type `make vhdl`  build the core specific files : 
   This will build the following files 
   - `gcd.vhd`  (the core GCD IP)
   - `gcd_cc.vhd` (the Avalon-MM wrapper)
   - `gcd_cc_hw.tcl` (the custom component description to be used by QSys)
   and copy them in the corresponding subdirs of this demo dir.
   
2. From the `sopc_demo` directory, run the `nios2_command_shell` script (located
   in `<altera>/nios2eds` directory, where `<altera>` is the root of the Intel/Altera software
   installation)

3. Launch the `QSys` tool : `make qsys`
   This will open the `QSys` SOPC editor. Check that the `gcd_cc` custom component is here and
   properly connected (it is named `gcd_cc_0`).
   Generate the HDL code : `QSys > Generate > Generate HDL`

4. Quit QSys, and from the Nios2 command shell invoke `make bitstream`
   This will compile the hardware descrition and build the bitstream to be loaded on the target FPGA 
   (this may take a few minutes)
   
5. Connect the target board and upload the hardware configuration :

   - `make hw` 

6. Build the software part (to be run on the Nios2 soft-core and invoking the GCD custom
   component) : `make bsp; make makef; make build`
   The source code is located in directory `./sw`.
   
7. Upload the compiled binary executable to the Nios2 and run it : `make run`
   Results will be displayed in the terminal.

