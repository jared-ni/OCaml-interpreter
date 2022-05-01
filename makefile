all: expr expr_test miniml

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

expr_test: expr_test.ml
	ocamlbuild -use-ocamlfind expr_test.byte

miniml: miniml.ml 
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte