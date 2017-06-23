#!/bin/sh
# Makefile for AtChem project

.SUFFIXES:
.SUFFIXES: .f90 .o
.PHONY: all test

ifeq ($(TRAVIS),true)
ifeq ($(TRAVIS_OS_NAME),linux)
# if linux, pass apt-get install location for cvode
F77      =  gfortran
CVODELIB=/home/travis/build/AtChem/AtChem/cvode/lib
OPENLIBMLIB=/home/travis/build/AtChem/AtChem/openlibm-0.4.1
RPATH_OPTION=-R
else
# if osx, then pass self-built cvode and homebrew gfortran
CVODELIB=/Users/travis/build/AtChem/AtChem/cvode/lib
F77=/usr/local/Cellar/gcc@4.8/4.8.5/bin/gfortran-4.8
OPENLIBMLIB=/Users/travis/build/AtChem/AtChem/openlibm-0.4.1
RPATH_OPTION=-rpath
endif
# else it's not on Travis, so check OS, and then pass local path to cvode and openlibm
else
OS := $(shell uname -s)
ifeq ($(OS),Linux)
testing=Linux
F77      =  gfortran
CVODELIB=/home/s/sc676/Sommariva/gcc/cvode/lib
OPENLIBMLIB=/home/s/sc676/Sommariva/AtChem/openlibm-0.4.1
RPATH_OPTION=-R
else
testing=OSX
F77      =  gfortran
CVODELIB=/Users/sam/ReSET/Sommariva/cvode/lib
OPENLIBMLIB=/Users/sam/git/atchem/openlibm-0.4.1
RPATH_OPTION=-rpath
endif
endif

# gfortran flags
FFLAGS   =  -ffree-form -fimplicit-none -Wall -Wpedantic -fcheck=all
LIBDIR   =  /usr/lib/:$(CVODELIB)
# Intel Fortran flags
#F77 = ifort
#FFLAGS   = -free -warn
#LIBDIR =

AOUT = atchem

all: $(AOUT)

LOCAL = makefile.$$(uname -n | perl -pe 's/\..+//')

makefile.local:
	touch $(LOCAL)
	ln -f $(LOCAL) makefile.local

include makefile.local

SRCS = src/dataStructures.f90 src/interpolationFunctions.f90 src/configFunctions.f90 src/inputFunctions.f90 src/outputFunctions.f90 src/atmosphereFunctions.f90 src/solarFunctions.f90 src/constraintFunctions.f90 src/solverFunctions.f90 src/parameterModules.f90 src/atchem.f90

LDFLAGS = -L$(CVODELIB) -L$(OPENLIBMLIB) -Wl,$(RPATH_OPTION),$(LIBDIR):$(OPENLIBMLIB) -lopenlibm -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial -lblas -llapack

# prerequisite is $(SRCS), so this will be rebuilt everytime any source file in $(SRCS) changes
$(AOUT): $(SRCS)
	$(F77) -o $(AOUT) $(SRCS) $(FFLAGS) $(LDFLAGS)
	@perl -ne 'm/\d+\.\d*[eE][-+]?\d+/ and push @a, "$$ARGV:$$.: $$&:\t$$_";END{@a and print("\nWARNING! Single-precision constants found:\n", @a)}' src/*.f90

# Search travis/tests/ for all subdirectories, which should reflect the full list of tests
TESTS := $(shell ls -d travis/tests/*/ | sed 's,travis/tests/,,g' | sed 's,/,,g')

test:
	@echo "Make: performing 'make clean'."
	@make clean
	@echo "Make: Running the following tests:" $(TESTS)
	@rm -f travis/tests/results
	@./travis/run_tests.sh "$(TESTS)" "$(CVODELIB):$(OPENLIBMLIB)"

clean:
	rm -f *.o
	rm -f $(AOUT)
	rm -f *.mod
	rm -f travis/tests/*/*.out travis/tests/*/*.output travis/tests/*/instantaneousRates/*[0-9]


# dependencies:

src/atchem.o : src/atchem.f90 src/inputFunctions.o src/configFunctions.o src/dataStructures.o
src/constraintFunctions.o : src/constraintFunctions.f90 src/dataStructures.o
src/atmosphereFunctions.o : src/atmosphereFunctions.f90
src/dataStructures.o : src/dataStructures.f90
src/inputFunctions.o : src/inputFunctions.f90 src/configFunctions.o src/dataStructures.o
src/interpolationFunctions.o : src/interpolationFunctions.f90 src/dataStructures.o
src/configFunctions.o : src/configFunctions.f90
src/outputFunctions.o : src/outputFunctions.f90 src/dataStructures.o
src/parameterModules.o : src/parameterModules.f90 src/dataStructures.o
src/solverFunctions.o : src/solverFunctions.f90 src/dataStructures.o
