FCF 
===

A small DSL for describing _Functional Compute Finite State Machines_ (FC-FSMs). FC-FSMs are
[Mealy-style synchronous FSMs](https://github.com/jserot/fsml) typically used for describing
computations to be implemented in hardware (on FPGAs for example).

Below is a graphical representation of an FC-FSM computing the GCD of two integers.
This FSM has two states (`idle` and `compute`), three inputs (`start`, `a` and `b`), one output
(`res`) and two internal variables (`m` and `n`).
This FSM is initially in state `idle`. When input `start` goes to `true`, it goes to state
`compute`, copying inputs `a` and `b` into variables `m` and `n` respectively and setting output
`rdy` to `false`. It remains in state `compute` while `m != n` (either substracting `n` from `m` or
`m` from `n`). It goes back to state `idle` when `m=n`, copying the final value of `m` -- which is
equal to `GCD(a,b)` -- into the output `res` and resetting `rdy` to `true`. All
transitions are implicitely triggered by a clock signal. 

![](https://github.com/jserot/fcf/blob/master/doc/figs/gcd.png "")

Here's a description of this FSM in FCF

```
let gcd (a, b) =
  let compute (m ,n) = 
  | m>n -> compute (m-n,n)
  | m<n -> compute (m,n-m)
  | m=n -> return m in
  compute (a,b)
;
```

In FCF, FSMs are viewed as _functions_ . Here the function `gcd` describing the corresponding FSM
has type `int * int -> int` (this type is automatically inferred). Each state is also viewed as a
function. Here the state `compute` is described by the (local) function `compute`, which also has
type `int * int -> int`. The transitions originating from the corresponding state are
described by three _guarded clauses_, of the form

```
<condition> -> <continuation>
```

where 
- `<condition>` is a boolean expression involving the inputs and local variables
- `<continuation>` is either a "jump" to another state, with a modification of the local variables,
  here denoted as a call to the corresponding function, or a final `return` with the associated
  value.

Note that the transitions from and to the `idle` state, and the handling of the `start` and `rdy`
  signals are made implicit in FCF.

Here's another example, in which the FSM has two computing states:

```
let is_even(n) =
  let even(m) = 
  | m>0 -> odd(m-1)
  | m<=0 -> return true
  and odd(m) = 
  | m>0 -> even(m-1)
  | m<=0 -> return false in
  even(n)
;
```

The `is_even` FSM returns `true` if its integer argument is even, `false` otherwise.
The corresponding diagram is

![](https://github.com/jserot/fcf/blob/master/doc/figs/even.png "")

FCF comes with facilities to declare and manipulate _algebraic data types_ (ADTs, aka _variants_) with a
powerful _pattern matching_ mechanism allowing transitions to inspect values of such types.

The following FSM description in FCF, for example, computes the sum of all elements of a list of
integers.

```
type 'a list = Nil | Cons of 'a * 'a list; 

let sum_list (a:int list) = 
  let f (l, acc) =
  | l~Nil -> return acc 
  | l~Cons(v,ll) -> f (ll, acc+v) in
  f (a,0)
;
```

The declaration of the `list` type follows the `OCaml` syntax : a list is either the empty list ,
`Nil`, or a `Cons` of value and a list, so that, for example, the list `[1;2;3]` is, classicaly,
represented by the value `Cons(1,Cons(2,Cons(3,Nil)))`.  Note that the `list` type is actually
polymorphic and can be used to declare lists of any type of values. The `sum_list` FSM has only one
state, `f`, with two parameters. The first parameter, `l` correspond to the input list. The second,
`acc`, is the accumulator used to compute the sum.  The two transitions of the `f` state _pattern
match_ against the `l` parameter using the following syntax

```
<expr>~<pattern>
```

If `l` is `Nil` (_i.e._ if the list is empty) then current value of the accumulator `acc` is
returned. Otherwise, the head `v` of the list is added to the accumulator and the state `f` is
applied to the tail `ll` of the list. 

Other examples using ADTs can be found in the
[`examples`](https://github.com/jserot/fcf/tree/master/examples) directory

Using
-----
  
The `fcfc` compiler provided in this package can

* produce graphical representations of FSMs described in FCF in the `.dot` format

* perform simulations of these FSMs; for example, running

```
fcfc -run -trace fact.fsm
```

where `fact.fsm` is the following program

```
let fact (n) =
  let compute (acc,k) = 
  | k<=n -> compute (acc*k,k+1)
  | k>n -> return acc in
  compute (1,1)
;

fact(5);
```

gives 

```
Eval compute(1,1) in env=[n=5]
Eval compute(acc*k,k+1) in env=[n=5,acc=1,k=1]
Eval compute(acc*k,k+1) in env=[n=5,acc=1,k=2]
Eval compute(acc*k,k+1) in env=[n=5,acc=2,k=3]
Eval compute(acc*k,k+1) in env=[n=5,acc=6,k=4]
Eval compute(acc*k,k+1) in env=[n=5,acc=24,k=5]
-: ? = 120
```


* generate VHDL code for simulation and synthesis; for example, here's the code generated for the
  `gcd` FSM from the FCF formulation given above.
  
```
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gcd is
  port(
        start: in std_logic;
        a: in unsigned(15 downto 0);
        b: in unsigned(15 downto 0);
        rdy: out std_logic;
        res: out unsigned(15 downto 0);
        clk: in std_logic;
        rst: in std_logic
);
end entity;

architecture RTL of gcd is
  type t_state is ( Idle, Compute );
  signal state: t_state;
  signal n: unsigned(15 downto 0);
  signal m: unsigned(15 downto 0);
begin
  process(rst, clk)
  begin
    if rst='1' then
      state <= Idle;
    elsif rising_edge(clk) then 
      case state is
      when Compute =>
        if ( m>n ) then
          m <= m-n;
          n <= n;
        elsif  ( m<n ) then
          m <= m;
          n <= n-m;
        elsif  ( m=n ) then
          res <= m;
          rdy <= '1';
          state <= Idle;
        end if;
      when Idle =>
        if ( start='1' ) then
          m <= a;
          n <= b;
          rdy <= '0';
          state <= Compute;
        end if;
    end case;
    end if;
  end process;
end architecture;
```

Installation
------------

To build from source, the pre-requisites are :

* `ocaml` (>= 4.10.0) with the following packages installed
  - `dune`
  - `menhir`

Download the source tree (`git clone https://github.com/jserot/fcf`).

From the root of the source tree :

1. `make`


Visualisation of the DOT diagrams requires the [`graphviz`](https://graphviz.org) package.

Compilation, simulation or synthesis of the code generated by the VHDL backend can be carried out by
any VHDL-93 compliant toolchain. We have used [`GHDL`](https://github.com/ghdl/ghdl) and [`Quartus
Prime`](https://www.intel.fr/content/www/fr/fr/software/programmable/quartus-prime/download.html)
(version 17.1).

Running the examples
--------------------

To try the examples, go the directory containing the example (*e.g.* `cd examples/gcd`) and type
`make <target>` where `<target>` may be
- `dot` (for generating a DOT diagram and viewing this diagram with `graphviz`)
- `run` (for simulating the program)
- `vhdl.code` (for generating VHDL code)
- `vhdl.run` (for compiling and simulating the generated VHDL code with `ghdl`)

For a complete list of capabilities and options : `fcfc --help`. 

*NOTE* : Under Linux and Windows, the `dotty` application supplied in the `graphviz` package is
buggy. The workaround is to generate the `.dot` file, convert it to the `gif` format using the
`dot` command and open the result file with any `gif` viewer. For example

```
make dot.code
dot -T gif -o my_fsm.gif my_fsm.dot
xv my_fsm.gif
```
