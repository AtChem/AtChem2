#!/bin/sh
# Makefile for AtChem project

.SUFFIXES:
.SUFFIXES: .f90 .o
.PHONY: all setup_var test

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

SRCS = dataStructures.f90 interpolationFunctions.f90 configFunctions.f90 inputFunctions.f90 outputFunctions.f90 atmosphereFunctions.f90 solarFunctions.f90 constraintFunctions.f90 solverFunctions.f90 parameterModules.f90 atchem.f90

LDFLAGS = -L$(CVODELIB) -L$(OPENLIBMLIB) -Wl,$(RPATH_OPTION),$(LIBDIR):$(OPENLIBMLIB) -lopenlibm -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial -lblas -llapack

# prerequisite is $(SRCS), so this will be rebuilt everytime any source file in $(SRCS) changes
$(AOUT): $(SRCS)
	$(F77) -o $(AOUT) $(SRCS) $(FFLAGS) $(LDFLAGS)
	@perl -ne 'm/\d+\.\d*[eE][-+]?\d+/ and push @a, "$$ARGV:$$.: $$&:\t$$_";END{@a and print("\nWARNING! Single-precision constants found:\n", @a)}' *.f90

# Search travis/tests/ for all subdirectories, which should reflect the full list of tests
TESTS := $(shell ls -d travis/tests/*/ | sed 's,travis/tests/,,g' | sed 's,/,,g')

test:
	@echo "Make: performing 'make clean'."
	@make clean
	@echo "Make: Running the following tests:" $(TESTS)
	@rm -f travis/tests/results
	@./travis/run_tests.sh "$(TESTS)" "$(CVODELIB):$(OPENLIBMLIB)"

.f90.o:
	$(F77) -c $(FFLAGS) $<

clean:
	rm -rf .libs
	rm -f *.lo
	rm -f *.o
	rm -f fortran_update.sh
	rm -f $(AOUT)
	rm -f fort.*
	rm -f *.mod
	rm -f travis/tests/*.out travis/tests/*.output travis/tests/*/*.output travis/tests/*/instantaneousRates/*[0-9]

webapp:
	bash ./make_webapp.sh


# dependencies:

atchem.o : atchem.f90 inputFunctions.o configFunctions.o dataStructures.o
constraintFunctions.o : constraintFunctions.f90 dataStructures.o
atmosphereFunctions.o : atmosphereFunctions.f90
dataStructures.o : dataStructures.f90
inputFunctions.o : inputFunctions.f90 configFunctions.o dataStructures.o
interpolationFunctions.o : interpolationFunctions.f90 dataStructures.o
configFunctions.o : configFunctions.f90
outputFunctions.o : outputFunctions.f90 dataStructures.o
parameterModules.o : parameterModules.f90 dataStructures.o
solverFunctions.o : solverFunctions.f90 dataStructures.o
