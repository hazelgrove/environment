CLIB := clib
OCAMLLIB := ocamllib

OCAML_DIR = $(shell ocamlc -where)

deps: 
	poetry install
	opam switch import opam.export --yes

change-deps:
	pip list --format=freeze > requirements.txt
	opam switch export opam.export

astclib: $(CLIB)/astlib.c
	gcc -shared -Wall -Werror -fPIC -o $(CLIB)/astclib.so \
	$(CLIB)/astlib.c $(CLIB)/ocamlInterface.c _build/default/$(OCAMLLIB)/libcinterface.so -lcurses \
	-L./_build/default/$(OCAMLLIB)/ -lcinterface \
	-Wl,-rpath,./_build/default/$(OCAMLLIB)/

watch: 
	cd $(OCAMLLIB) && dune build @fmt --auto-promote --watch && cd ../

astparser:
	ocamlbuild -use-menhir main.byte -I ocamllib/astparser

ocamlinterface:
	eval $(opam env)
	cd $(OCAMLLIB) && dune build && cd ../

astenv:
	make ocamlinterface
	make astclib

ocamltest:
	dune runtest

clean:
	cd $(OCAMLLIB) && dune clean && cd ../
	rm -f $(CLIB)/*.so $(CLIB)/*.o $(CLIB)/*.a $(OCAMLLIB)/*.cmi $(OCAMLLIB)/*.cmx $(OCAMLLIB)/*.cmo $(OCAMLLIB)/*.cmt $(OCAMLLIB)/*.o $(OCAMLLIB)/astparser/*.cmo $(OCAMLLIB)/astparser/*.cmi
