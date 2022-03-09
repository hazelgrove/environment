CLIB := clib
OCAMLLIB := ocamllib

OCAML_DIR = $(shell ocamlc -where)

ctest: 
	gcc -o $(CLIB)/test -g $(CLIB)/test.c -I $(OCAML_DIR) $(CLIB)/astlib.c $(CLIB)/ocamlInterface.c $(OCAMLLIB)/_build/default/cinterface.exe.o -L`ocamlc -where` -lcamlrun

astclib: $(CLIB)/astlib.c
	gcc -shared -Wall -Werror -o $(CLIB)/astclib.so -I $(OCAML_DIR) $(CLIB)/astlib.c $(CLIB)/ocamlInterface.c $(OCAMLLIB)/_build/default/cinterface.exe.o -lcurses

testclib: $(CLIB)/test.c
	gcc -shared -Wall -Werror -o $(CLIB)/test.so $(CLIB)/test.c -lcurses

astparser: $(OCAMLLIB)/astparser/ast.ml $(OCAMLLIB)/astparser/lexer.mll $(OCAMLLIB)/astparser/parser.mly $(OCAMLLIB)/astparser/main.ml
	ocamlbuild -use-menhir main.byte -I ocamllib/astparser

ocamltest: $(OCAMLLIB)/test.ml
	ocamlopt -output-obj $(OCAMLLIB)/test.ml -o $(OCAMLLIB)/test.o

OCAML_TARGET := cinterface
ocamlinterface: $(OCAMLLIB)/$(OCAML_TARGET).ml $(CLIB)/ocamlInterface.c
	cd $(OCAMLLIB) && dune build && cd ../

astenv:
	make ocamlinterface
	make astclib

clean:
	cd $(OCAMLLIB) && dune clean && cd ../
	rm -f $(CLIB)/*.so $(CLIB)/*.o $(CLIB)/*.a $(OCAMLLIB)/*.cmi $(OCAMLLIB)/*.cmx $(OCAMLLIB)/*.cmo $(OCAMLLIB)/*.cmt $(OCAMLLIB)/*.o $(OCAMLLIB)/astparser/*.cmo $(OCAMLLIB)/astparser/*.cmi