This is a reformulation of ../../asum using a local array variable.
Note that the array declaration does _not_ initialize it.
**Warning** : uninitialized variable may cause evaluation errors (usually reported as a fatal
`decode_xxx: illegal argument` error
**Note** : the array size is here hardwired in the code; ideally it should be equal to the `n`
parameter of the `foo` FSM; this is currently not supported
