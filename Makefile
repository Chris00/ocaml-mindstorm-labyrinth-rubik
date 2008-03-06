# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../mindstorm
DOC_DIR=doc

OCAMLC_FLAGS = -thread -g -dtypes -custom -I $(MINDSTORM_PATH)
OCAMLOPT_FLAGS = -thread -dtypes -I $(MINDSTORM_PATH)

INTERFACES=$(wildcard *.mli) \
  $(wildcard labyrinth/*.mli) $(wildcard rubik/*.mli)
TESTS=$(wildcard *-*.ml)
LIBS_CMA=unix.cma mindstorm.cma threads.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: all byte native tests tests-byte test-native
all: byte native
	$(CD) labyrinth/ &&  $(MAKE)
	$(CD) rubik/ &&  $(MAKE)
byte: robot.cma
native: robot.cmxa
tests: tests-byte test-native
tests-byte: $(TESTS:.ml=.exe)
test-native: $(TESTS:.ml=.com)

run-light.exe: robot.cmo
run-push.exe: robot.cmo
run-turn.exe: robot.cmo
run-look.exe: robot.cmo
run-follow-line.exe: robot.cmo

# General "event" library
robot.cma: robot.cmo
robot.cmxa: robot.cmx


# Generate HTML documentation
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	-$(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -I $(MINDSTORM_PATH) -I labyrinth/ -I rubik/

# Add subdirectories (necessary to compile the doc of all modules)
.depend.ocaml: $(wildcard labyrinth/*.ml) $(wildcard labyrinth/*.mli)
.depend.ocaml: $(wildcard rubik/*.ml) $(wildcard rubik/*.mli)

labyrinth/%:
	cd labyrinth/ && $(MAKE) $(@F)
rubik/%:
	cd rubik/ && $(MAKE) $(@F)

include Makefile.ocaml

clean::
	$(RM) *.exe *.com
	-$(CD) $(DOC_DIR); $(RM) *~ *.html *.css
	$(CD) labyrinth/ &&  $(MAKE) clean
	$(CD) rubik/ &&  $(MAKE) clean
