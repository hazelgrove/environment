CLIB := ./clib
OCAMLLIB := ./ocamllib

OCAML_DIR = $(shell ocamlc -where)

astclib: $(CLIB)/astlib.c
	gcc -shared -Wall -Werror -fpic $(CLIB)/astlib.c -o $(CLIB)/astlib.so

testclib: $(CLIB)/test.c
	gcc -shared -Wall -Werror -o $(CLIB)/test.so -I $(OCAML_DIR) $(CLIB)/test.c $(CLIB)/ocamlInterface.a -lcurses

astparser: $(OCAMLLIB)/astparser.ml 
	ocamlc -o astparser astparser.ml -I +compiler-libs ocamlcommon.cma

ocamltest: $(OCAMLLIB)/test.ml
	ocamlopt -output-obj $(OCAMLLIB)/test.ml -o $(OCAMLLIB)/test.o

OCAML_TARGET := test
ocamlinterface: $(OCAMLLIB)/$(OCAML_TARGET).ml $(CLIB)/ocamlInterface.c
	ocamlc -custom -output-obj -o $(OCAMLLIB)/$(OCAML_TARGET).o $(OCAMLLIB)/$(OCAML_TARGET).ml
	ocamlc -c $(CLIB)/ocamlInterface.c -o $(CLIB)/ocamlInterface.o
	cp $(OCAML_DIR)/libcamlrun.a $(CLIB)/ocamlInterface.a && chmod +w $(CLIB)/ocamlInterface.a
	ar r $(CLIB)/ocamlInterface.a $(CLIB)/ocamlInterface.o $(OCAMLLIB)/$(OCAML_TARGET).o

clean:
	rm -f $(CLIB)/*.so $(CLIB)/*.o $(CLIB)/*.a $(OCAMLLIB)/*.cmi $(OCAMLLIB)/*.cmx $(OCAMLLIB)/*.cmo $(OCAMLLIB)/*.o