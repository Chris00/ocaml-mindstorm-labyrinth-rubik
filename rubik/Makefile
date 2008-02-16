# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../../mindstorm
DOC_DIR = doc

OCAMLC_FLAGS = -thread -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH)
OCAMLOPT_FLAGS = -thread -dtypes -I $(MINDSTORM_PATH)

INTERFACES=$(wildcard *.mli)
PROGRAM=
LIBS_CMA=unix.cma mindstorm.cma threads.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(PROGRAM:.ml=.exe)
native: $(PROGRAM:.ml=.com)


test_ppm.exe: ppm.cmo
test_ppm.exe: LIBS_CMA+=graphics.cma

# Generate HTML documentation for these modules only
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	test -d $(DOC_DIR) || $(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -I $(MINDSTORM_PATH)

include ../Makefile.ocaml

clean::
	$(RM) *.exe *.com
	$(RM) -rf $(DOC_DIR)