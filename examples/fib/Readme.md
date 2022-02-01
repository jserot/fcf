The classical Fibonacci example.
Note the classical definition 

```
fib(n) = if n=0 or n=1 then 1 else fib(n-2) + fib(n-1)
```

has to be rewritten in _tail recursive_ form :

```
fib(n) = f(1,1,n)
  where f(a,b,n) = if n > 1 then f(a+b,a,n-1) else 1
```
