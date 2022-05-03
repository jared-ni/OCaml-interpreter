all: expr expr_test miniml evaluation_test evaluation

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

expr_test: expr_test.ml
	ocamlbuild -use-ocamlfind expr_test.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

evaluation_test: evaluation_test.ml
	ocamlbuild -use-ocamlfind evaluation_test.byte

miniml: miniml.ml 
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte