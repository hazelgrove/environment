CLIB := ./clib
OCAMLLIB := ./ocamllib

OCAML_DIR = $(shell ocamlc -where)

astclib: $(CLIB)/astlib.c
	gcc -shared -Wall -Werror -o $(CLIB)/astclib.so -I $(OCAML_DIR) $(CLIB)/astlib.c $(CLIB)/ocamlInterface.a -lcurses

testclib: $(CLIB)/test.c
	gcc -shared -Wall -Werror -o $(CLIB)/test.so -I $(OCAML_DIR) $(CLIB)/test.c $(CLIB)/ocamlInterface.a -lcurses

astparser: $(OCAMLLIB)/astparser.ml
	ocamlopt -I +compiler-libs -bin-annot -g ocamlcommon.cmxa ocamloptcomp.cmxa $(OCAMLLIB)/astparser.ml -o $(OCAMLLIB)/astparser

ocamltest: $(OCAMLLIB)/test.ml
	ocamlopt -output-obj $(OCAMLLIB)/test.ml -o $(OCAMLLIB)/test.o

OCAML_TARGET := astlib
ocamlinterface: $(OCAMLLIB)/$(OCAML_TARGET).ml $(CLIB)/ocamlInterface.c
	ocamlc -custom -output-obj -o $(OCAMLLIB)/$(OCAML_TARGET).o $(OCAMLLIB)/$(OCAML_TARGET).ml
	ocamlc -c $(CLIB)/ocamlInterface.c -o $(CLIB)/ocamlInterface.o
	cp $(OCAML_DIR)/libcamlrun.a $(CLIB)/ocamlInterface.a && chmod +w $(CLIB)/ocamlInterface.a
	ar r $(CLIB)/ocamlInterface.a $(CLIB)/ocamlInterface.o $(OCAMLLIB)/$(OCAML_TARGET).o

astenv:
	make ocamlinterface
	make astclib

clean:
	rm -f $(CLIB)/*.so $(CLIB)/*.o $(CLIB)/*.a $(OCAMLLIB)/*.cmi $(OCAMLLIB)/*.cmx $(OCAMLLIB)/*.cmo $(OCAMLLIB)/*.cmt $(OCAMLLIB)/*.o