# Compile in -cunstom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../mindstorm

OCAMLC_FLAGS = -thread -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH)
OCAMLOPT_FLAGS = -thread -dtypes -I $(MINDSTORM_PATH)

TESTS=$(wildcard run_*.ml)
LIBS_CMA=unix.cma mindstorm.cma threads.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)

run_light.exe: robot.cmo
run_push.exe: robot.cmo
run_turn.exe: robot.cmo


# General "event" library
robot.cma: robot.cmo
robot.cmxa: robot.cmx

include Makefile.ocaml

clean::
	$(RM) *.exe *.com