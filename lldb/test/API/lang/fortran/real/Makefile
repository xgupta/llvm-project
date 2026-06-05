FC ?= flang
FFLAGS ?= -g -O0

a.out: real.f90
	$(FC) $(FFLAGS) real.f90 -o a.out

include Makefile.rules