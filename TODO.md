- allow state parameters to shadow FSM args. Currently, a declaration like
```
let foo (x:t) = 
  let f (x,...) =
  | ...
  f (x,...)
;
```
gives an error

- fix monophormisation bug; see `examples/list/list_rev` : it shouldn't be necessary (?) to add type
  signatures to the parameters of state `f` 

- rebuild heap before each computation (instead of building it once for all at init time)

- add a type coercion operator (ex: `(x as t)` or `x:t`)

- fix results in `cordic/vfx` (overflow ? sign-ness ?)

- fix `fcf-mode.el`

- script for regression testing
