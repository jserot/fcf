## VHDL backend

- support for Qsys generation for FSMs using heap allocation (ex: `examples/option`)
- add ROM inference for `examples/cordic/vfx` (see:
  https://www.intel.com/content/www/us/en/docs/programmable/683082/21-3/inferring-rom-functions-from-hdl-code.html)

## Typing

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 

## Apps

- sieve
- eval_exp
- list_rev with local array
