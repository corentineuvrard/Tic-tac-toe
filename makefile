EXEC = morpion

LIBS = /usr/lib/ocaml/graphics.cma

all:
	ocamlc -c variables.ml
	ocamlc -c drawings.ml
	ocamlc -c functions.ml
	ocamlc -c ai.ml
	ocamlc -c tic_tac_toe.ml
	ocamlc -o $(EXEC) $(LIBS) variables.cmo drawings.cmo functions.cmo ai.cmo tic_tac_toe.cmo

clean:
	rm -fr *.cmi *.cmo
	rm -f $(EXEC)
