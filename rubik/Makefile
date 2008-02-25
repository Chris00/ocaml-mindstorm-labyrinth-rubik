# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../../mindstorm
DOC_DIR = doc

OCAMLC_FLAGS = -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH) -I ..
OCAMLOPT_FLAGS = -dtypes -I $(MINDSTORM_PATH) -I ..
PP=camlp4o pa_macro.cmo #Camlp4MacroParser.cmo

INTERFACES=$(wildcard *.mli)
PROGRAM=run.ml
LIBS_CMA=unix.cma bigarray.cma mindstorm.cma robot.cma
LIBS_CMXA=$(patsubst %cmo,%.cmx, $(patsubst %.cma,%.cmxa, $(LIBS_CMA)))

.PHONY: all byte native
all: byte native
byte: $(PROGRAM:.ml=.exe)
native: $(PROGRAM:.ml=.com)

run.exe: rubik.cmo
run.com: rubik.cmx

display.cmo: rubik.cmo
display.cmo: LIBS_CMA+= graphics.cma

display.exe: rubik.cmo
display.exe: LIBS_CMA+= graphics.cma

test_ppm.exe: ppm.cmo
test_ppm.exe: LIBS_CMA+=graphics.cma

# Generate HTML documentation for these modules only
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	test -d $(DOC_DIR) || $(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -I $(MINDSTORM_PATH)

include ../Makefile.ocaml
include ../Makefile.latex

# Images -> talk (requires ImageMagick)
images/Rubik_cube.jpg: images/Rubik_cube.svg
	cd images/ && convert $^ $@

clean::
	$(RM) *.exe *.com images/Rubik_cube.jpg
	$(RM) -rf $(DOC_DIR)