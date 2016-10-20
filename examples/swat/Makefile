SHELL = /bin/bash

SOURCES=swat.ml
CMOS=$(SOURCES:.ml=.cmo)

.SUFFIXES: .ml .cmx .out

.PHONY : swat landuse calibration clean all

all : swat 
swat : swat.cmx
clean : 
	rm -rf *.cmo *.cmi *.o *.cmx *.out

swat.cmx: $(SOURCES)
	ocamlfind ocamlopt -c  -package core,pads,pads.ppx,re,unix,str,re.glob swat.ml -thread -w -30

%.cmx: %.ml swat.cmx
	ocamlfind ocamlopt -c  -package core,pads,pads.ppx,re,unix,str,re.glob  $< -thread -w -30

%.out: %.cmx
	ocamlfind ocamlopt -o $@ -linkpkg -package core,pads,pads.ppx,re,unix,str,re.glob  swat.cmx $< -thread -w -30


