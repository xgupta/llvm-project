FC ?= flang
FFLAGS ?= -g -O0

a.out: frame.f90
	$(FC) $(FFLAGS) frame.f90 -o a.out

include Makefile.rules