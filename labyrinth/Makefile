head	1.1;
access;
symbols;
locks; strict;
comment	@# @;


1.1
date	2008.02.14.10.51.27;	author marku;	state Exp;
branches;
next	;


desc
@Makefile of the labyrinth directory
@


1.1
log
@Initial revision
@
text
@# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
MINDSTORM_PATH = ../../mindstorm

OCAMLC_FLAGS = -thread -g -dtypes -dllpath .. -custom -I $(MINDSTORM_PATH)
OCAMLOPT_FLAGS = -thread -dtypes -I $(MINDSTORM_PATH)

INTERFACES=$(wildcard *.mli)
PROGRAM=
LIBS_CMA=unix.cma mindstorm.cma threads.cma
LIBS_CMXA=$(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)

include ../Makefile.ocaml

clean::
	$(RM) *.exe *.com@
