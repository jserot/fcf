# v0.5 - Jul XX, 2022

- **changed syntax** for toplevel declarations :
  - FSMs are now introduced using the `fsm` keyword (instead of `let`)
  - instances are introduced using the `let` keyword and their result(s) can be named and used as
  argument in subsequent `let` instances (ex: `let r = f1 (x1); let r2 = f2(r1)`). See `examples/compose` for ex.
  
# v0.4 - Jun 27, 2022

- (experimental) support for FSM local variables (see `examples/lvars`)
- added ranged int type (ex: `int<0:20>`)
- added type coercion operator (ex: `i:>unsigned<8>`); currently only works for int types 
- (*the two latter additions are mainly useful for generating efficient and synthetisable VHDL code*)

# v0.3 - Mar 19, 2022

- support for algebraic data types down to the VHDL backend (see for ex. `examples/{option,option2,list}`)

# v0.2 - Jan 20, 2022

- support for multi-results FSMs (see for ex. `examples/cordic/vfl`)
- support for global constants and (immutable) arrays (see for ex. `examples/{asum,cordic}`)
- removed `-vhdl_numeric_std` option; sized integers (`[un]signed<n>`) are now systematically
  translated to VHDL `[un]unsigned` and default integers (`int`) to VHDL `integer`
- the VHDL backend can now produce a _testbench_ for simulating the generated code (see for ex. `examples/gcd`)
- support for Intel QSys SOPC generation 
- default file suffix for source file is now `.fcf` (instead of `.fsm`)
- added an `emacs` mode (`fcf-mode.el`) for editing source files
- added CORDIC examples (with fixed-point and floating-point versions)
- added BABBAGE examples 

# v0.1 - Jul 28, 2021

- Initial version
