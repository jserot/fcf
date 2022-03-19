## Language

- allow state parameters to shadow FSM args. Currently, a declaration like
```
let foo (x:t) = 
  let f (x,...) =
  | ...
  f (x,...)
;
```
gives an error

- add a type coercion operator (`x:unsigned<8> ... (x as int)`)

## Typing

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 

## VHDL backend

- rebuild heap before each computation (instead of building it once for all at init time)

## Examples

- fix results in `cordic/vfx` (overflow ? sign-ness ?)

## Etc

- fix `fcf-mode.el`
