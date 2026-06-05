FC ?= flang
FFLAGS ?= -g -O0

a.out: integers.f90
	$(FC) $(FFLAGS) integers.f90 -o a.out

include Makefile.rules