Evaluating degree-2 polynomials (ax^2+bx+c) using the Babbage "difference engine" method.

The classical recurrent equations :

       
```
f(n) = if n=0 then c else f(n-1) + g(n)
```


```
g(n) = if n=0 then b-a else g(n-1) + 2a
```

are here re-written in tail-recursive form, each one giving a distinct state
