# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../../mindstorm
DOC_DIR = doc

OCAMLC_FLAGS = -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH) -I ..
OCAMLOPT_FLAGS = -dtypes -I $(MINDSTORM_PATH) -I ..

INTERFACES=$(wildcard *.mli)
PROGRAM=run.ml
LIBS_CMA=unix.cma mindstorm.cma robot.cmo graphics.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(PROGRAM:.ml=.exe)
native: $(PROGRAM:.ml=.com)

run.exe: labyrinth.cmo display.cmo solver.cmo


# Generate HTML documentation for these modules only
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	test -d $(DOC_DIR) || $(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -I $(MINDSTORM_PATH)


# Define the OS type for RM,...
../.os_type: ../make_os_type.exe
	"$<" > $@
include ../.os_type

include ../Makefile.ocaml

clean::
	-$(RM) $(wildcard *.exe *.com)
	-$(RM) -rf $(DOC_DIR)