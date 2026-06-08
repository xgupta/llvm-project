FC ?= flang
FFLAGS ?= -g -O0

a.out: frame.f90
	$(FC) $(FFLAGS) $< -o a.out
	
include Makefile.rules