## Language

- add a type coercion operator (`x:unsigned<8> ... (x as int)`)

## Compiler

- option `-vhdl_dump_heap` should dump heap obly after heap init and allocation (by ctors)

## Typing

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 

## Examples

- fix `_tofix/tree` 
