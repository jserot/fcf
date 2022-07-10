## VHDL backend

- support for Qsys generation for FSMs using heap allocation (ex: `examples/option`)
- add ROM inference for `examples/cordic/vfx` (see:
  https://www.intel.com/content/www/us/en/docs/programmable/683082/21-3/inferring-rom-functions-from-hdl-code.html)
- support for global (shared btw FSMs) heap; this is required for implementing dependent toplevel
  let-bindings when the bound values are heap-allocated (ex: `let l1 = list_rev ...; let l2 =
  list_map l1`); in the current version, this type of dependent `let` definitions is only supported
  when the bound values are scalars (see for ex. `examples/compose`)

## Typing

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 

## Apps

- list_rev with local array
- sieve (also with local array)
