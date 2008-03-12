# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../../mindstorm
DOC_DIR = doc

OCAMLC_FLAGS = -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH) -I ..
OCAMLOPT_FLAGS = -dtypes -I $(MINDSTORM_PATH) -I ..
CAMLP4 = $(shell which camlp4of.opt || echo camlp4o pa_macro.cmo)
PP = $(CAMLP4) $(D_OS) #Camlp4MacroParser.cmo

INTERFACES=$(wildcard *.mli)
PROGRAM=run.ml
LIBS_CMA=unix.cma bigarray.cma mindstorm.cma robot.cma 
LIBS_CMXA=$(patsubst %cmo,%.cmx, $(patsubst %.cma,%.cmxa, $(LIBS_CMA)))

.PHONY: all byte native
all: byte native
byte: $(PROGRAM:.ml=.exe)
native: $(PROGRAM:.ml=.com)

RUN_LIBS=rubik.ml ida.ml movement.ml translator.ml display.ml solver.ml \
  ppm.ml snapshot.ml init_color.ml
run.exe: LIBS_CMA+= graphics.cma
run.exe: $(RUN_LIBS:.ml=.cmo)
run.com: LIBS_CMXA+= graphics.cmxa
run.com: $(RUN_LIBS:.ml=.cmx)

a_star.cmo: rubik.cmo priority_queue.cmo

display.exe: rubik.cmo
display.exe: LIBS_CMA+= graphics.cma

test_ppm.exe: ppm.cmo
test_ppm.exe: LIBS_CMA+=graphics.cma

translator.cmo : movement.cmo

init_color.exe : rubik.cmo display.cmo ppm.cmo snapshot.cmo movement.cmo translator.cmo
init_color.exe : LIBS_CMA+=graphics.cma

calibration.exe : rubik.cmo ppm.cmo snapshot.cmo movement.cmo translator.cmo display.cmo init_color.cmo
calibration.exe : LIBS_CMA+=graphics.cma

test_movement.exe: movement.cmo

test_translator.exe : movement.cmo translator.cmo

test_snapshot.exe : ppm.cmo snapshot.cmo
test_snapshot.exe : LIBS_CMA+=graphics.cma

# Generate HTML documentation for these modules only
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	-$(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html \
	  $(INTERFACES) -I $(MINDSTORM_PATH)

include ../Makefile.ocaml
include ../Makefile.latex
# Define the OS type for the Camlp4 preprocessor,...
../.os_type: ../make_os_type.exe
	"$<" > $@
include ../.os_type

# Images -> talk (requires ImageMagick)
images/Rubik_cube.jpg: images/Rubik_cube.svg
	cd images/ && convert $^ $@

clean::
	$(RM) *.exe *.com *.obj *.lib images/Rubik_cube.jpg
	$(RM) -rf $(DOC_DIR)
