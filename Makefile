# Compile in -cunstom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../mindstorm

OCAMLC_FLAGS = -thread -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH)
OCAMLOPT_FLAGS = -thread -dtypes -I $(MINDSTORM_PATH)

TESTS=$(wildcard *-*.ml)
LIBS_CMA=unix.cma mindstorm.cma threads.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)

run-light.exe: robot.cmo
run-push.exe: robot.cmo
run-turn.exe: robot.cmo
run-look.exe: robot.cmo
run-follow-line.exe: robot.cmo

# General "event" library
robot.cma: robot.cmo
robot.cmxa: robot.cmx

include Makefile.ocaml

clean::
	$(RM) *.exe *.com