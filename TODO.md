- allow state parameters to shadow FSM args. Currently, a declaration like
```
let foo (x:t) = 
  let f (x,...) =
  | ...
  f (x,...)
;
```
gives an error

- fix `fcf-mode.el`
