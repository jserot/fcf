## VHDL backend

- fix bug in `.tcl` file generation in `examples/lvars/asum2` (add `globals.vhd`, remove `fcf` lib
  and packages)
- fix bug in generated VHDL code in `examples/lvars/asum2` (some types and related conversion fns
  are wrong; see `.../SF2/vhdl/fcf/asum2/qsys` for the right code)

## Typing

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 
