FC ?= flang
FFLAGS ?= -g -O0

a.out: logical.f90
	$(FC) $(FFLAGS) logical.f90 -o a.out

include Makefile.rules