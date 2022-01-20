# v0.2 - TBA

- support for multi-results FSMs (see for ex. `examples/cordic/vfl`)
- support for global constants and (immutable) arrays (see for ex. `examples/{asum,cordic}`)
- removed `-vhdl_numeric_std` option; sized integers (`[un]signed<n>`) are now systematically
  translated to VHDL `[un]unsigned` and default integers (`int`) to VHDL `integer`
- the VHDL backend can now produce a _testbench_ for simulating the generated code (see for ex. `examples/gcd`)
- support for Intel QSys SOPC generation 
- default file suffix for source file is now `.fcf` (instead of `.fsm`)
- added an `emacs` mode (`fcf-mode.el`) for editing source files
- added CORDIC examples (with fixed-point and floating-point versions)
  

# v0.1 - Jul 28, 2021

- Initial version
