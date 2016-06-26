all: opam
	ocamlfind ocamlopt -package yojson -linkpkg main.ml
opam:
	eval `opam config env`

clean:
	rm *.cm? *.o
