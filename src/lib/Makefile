all: byte # native

byte:
	dune build fcf.cma

native:
	dune build fcf.cmxa

doc:
	dune build @doc

clean:
	dune clean

clobber: clean
	@rm -f *~
