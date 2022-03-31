CLIB := clib
OCAMLLIB := ocamllib

OCAML_DIR = $(shell ocamlc -where)

deps: 
	pip install -r requirements.txt
	opam switch import opam.export --yes

change-deps:
	pip list --format=freeze > requirements.txt
	opam switch export opam.export

astclib: $(CLIB)/astlib.c
	gcc -shared -Wall -Werror -fPIC -o $(CLIB)/astclib.so \
	$(CLIB)/astlib.c $(CLIB)/ocamlInterface.c $(OCAMLLIB)/_build/default/libcinterface.so -lcurses \
	-L./$(OCAMLLIB)/_build/default -lcinterface \
	-Wl,-rpath,./$(OCAMLLIB)/_build/default

astparser:
	ocamlbuild -use-menhir main.byte -I ocamllib/astparser

ocamlinterface:
	cd $(OCAMLLIB) && dune build && cd ../

astenv:
	make ocamlinterface
	make astclib

ocamltest:
	cd $(OCAMLLIB) && dune runtest && cd ../

clean:
	cd $(OCAMLLIB) && dune clean && cd ../
	rm -f $(CLIB)/*.so $(CLIB)/*.o $(CLIB)/*.a $(OCAMLLIB)/*.cmi $(OCAMLLIB)/*.cmx $(OCAMLLIB)/*.cmo $(OCAMLLIB)/*.cmt $(OCAMLLIB)/*.o $(OCAMLLIB)/astparser/*.cmo $(OCAMLLIB)/astparser/*.cmi