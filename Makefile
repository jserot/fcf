.PHONY: doc

all: build 

build:
	dune build src/lib/fcf.cma
	dune build src/bin/fcfc.bc

install:
	dune build @install

INSTALL_DOCDIR=`opam config var doc`

opam.install: 
	opam install .

opam.remove:
	opam remove .

opam.show:
	opam info rfsm

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc:
	dune build @doc
	(cd ./doc; make)

html: README.md
	pandoc -t html -o README.html README.md
	pandoc -t html -o CHANGES.html CHANGES.md

toplevel:
	dune exec ./src/bin/fcfc.exe

tests:
	(cd examples; ./do_tests dot)
	(cd examples; ./do_tests sim)

clean:
	dune clean
	\rm -f README.html CHANGES.html

clobber: clean
	\rm -f *~


