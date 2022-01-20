Evaluating degree-2 polynomials (ax^2+bx+c) using the Babbage "difference engine" method.

The classical recurrent equations :

       | c  if n=0
f(n) = |
       | f(n-1) + g(n)


       | b-a  if n=0
g(n) = |
       | g(n-1) + 2a

are here re-written in tail-recursive form, each one giving a distinct state
