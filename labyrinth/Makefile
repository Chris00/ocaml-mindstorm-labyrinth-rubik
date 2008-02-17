# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../../mindstorm

OCAMLC_FLAGS = -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH) -I ..
OCAMLOPT_FLAGS = -dtypes -I $(MINDSTORM_PATH) -I ..

INTERFACES=$(wildcard *.mli)
PROGRAM=
LIBS_CMA=unix.cma mindstorm.cma robot.cmo
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(PROGRAM:.ml=.exe)
native: $(PROGRAM:.ml=.com)


# Generate HTML documentation for these modules only
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	test -d $(DOC_DIR) || $(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -I $(MINDSTORM_PATH)

include ../Makefile.ocaml

clean::
	$(RM) *.exe *.com