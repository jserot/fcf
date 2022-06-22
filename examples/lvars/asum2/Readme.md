This is a reformulation of ../../asum using a local array variable.

**Notes**
- the array `a` is (deliberately) not initialized (this is unsafe in the general case but ok here)
- the array size is here hardwired in the code; ideally it should be equal to the `n` parameter of
  the `foo` FSM; this is currently not supported
- some specific encodings here ensures that the `Quartus` synthetizer will implement the `a` array
  as RAM block (s) (and _not_ logic elements); namely
  - the size of the array `a` is hardcoded (`128`)
  - the type of its elements is `unsigned<..>` (or `signed<..>`); it seems that declaring an array
    with `integer` elements precludes RAM inference (tested under Quartus 17.1)
  - the type of the state parameter `i`, used to index the `a` array, is a _ranged integer_;
    declaring it as simple integer also precludes RAM inference; as a result, the _type coercion_
    operator (`:>`) is required when using `i` to initialize the array elements
