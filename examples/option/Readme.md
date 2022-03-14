A (silly) example illustrating computations on values with (here non recursive) algebraic data types.
The `silly` FCF computes the following recursive function :
```
  f(None) = 0
  f(Some 0) = None
  f(Some n) = Some (n-1) for n>0
```
The .fcf encoding uses the polymorphic algebraic type `option` and _pattern matching_ (with the
`expr~pattern` syntax).
  
