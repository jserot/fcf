## VHDL backend

- support for Qsys generation for FSMs using heap allocation (ex: `examples/option`)

## Typing

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 
