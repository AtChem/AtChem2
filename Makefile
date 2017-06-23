#!/bin/sh
# Makefile for AtChem project

.SUFFIXES:
.SUFFIXES: .f90 .o
.PHONY: all test

OS := $(shell uname -s)

ifeq ($(TRAVIS),true)
ifeq ($(TRAVIS_OS_NAME),linux)
# if linux, pass apt-get install location for cvode
FORT_COMP    = gfortran
CVODELIB     = /home/travis/build/AtChem/AtChem/cvode/lib
else
# if osx, then pass self-built cvode and homebrew gfortran
CVODELIB     = /Users/travis/build/AtChem/AtChem/cvode/lib
FORT_COMP    = /usr/local/Cellar/gcc@4.8/4.8.5/bin/gfortran-4.8
endif
# else it's not on Travis, so check OS, and then pass local path to cvode and openlibm
else
ifeq ($(OS),Linux)
FORT_COMP    = gfortran
CVODELIB     = /home/s/sc676/Sommariva/gcc/cvode/lib
else
FORT_COMP    = gfortran
CVODELIB     = /Users/sam/ReSET/Sommariva/cvode/lib
endif
endif

ifeq ($(OS),Linux)
RPATH_OPTION = -R
else
RPATH_OPTION = -rpath
endif

OPENLIBMDIR  = openlibm-0.4.1

# gfortran flags
FFLAGS   =  -ffree-form -fimplicit-none -Wall -Wpedantic -fcheck=all
LIBDIR   =  /usr/lib/:$(CVODELIB)
# Intel Fortran flags
#FORT_COMP = ifort
#FFLAGS   = -free -warn
#LIBDIR =

OBJ = obj
SRC = src

AOUT = atchem

all: $(AOUT)

LOCAL = makefile.$$(uname -n | perl -pe 's/\..+//')

makefile.local:
	touch $(LOCAL)
	ln -f $(LOCAL) makefile.local

include makefile.local

SRCS = $(SRC)/dataStructures.f90 $(SRC)/interpolationFunctions.f90 $(SRC)/configFunctions.f90 $(SRC)/inputFunctions.f90 $(SRC)/outputFunctions.f90 $(SRC)/atmosphereFunctions.f90 $(SRC)/solarFunctions.f90 $(SRC)/constraintFunctions.f90 $(SRC)/solverFunctions.f90 $(SRC)/parameterModules.f90 $(SRC)/atchem.f90

LDFLAGS = -L$(CVODELIB) -L$(OPENLIBMDIR) -Wl,$(RPATH_OPTION),$(LIBDIR):$(OPENLIBMDIR) -lopenlibm -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial -lblas -llapack

# prerequisite is $(SRCS), so this will be rebuilt everytime any source file in $(SRCS) changes
$(AOUT): $(SRCS)
	$(FORT_COMP) -o $(AOUT) -J$(OBJ) -I$(OBJ) $(SRCS) $(FFLAGS) $(LDFLAGS)
	@perl -ne 'm/\d+\.\d*[eE][-+]?\d+/ and push @a, "$$ARGV:$$.: $$&:\t$$_";END{@a and print("\nWARNING! Single-precision constants found:\n", @a)}' $(SRC)/*.f90

# Search travis/tests/ for all subdirectories, which should reflect the full list of tests
TESTS := $(shell ls -d travis/tests/*/ | sed 's,travis/tests/,,g' | sed 's,/,,g')

test:
	@echo "Make: performing 'make clean'."
	@make clean
	@echo "Make: Running the following tests:" $(TESTS)
	@rm -f travis/tests/results
	@./travis/run_tests.sh "$(TESTS)" "$(CVODELIB):$(OPENLIBMDIR)"

clean:
	rm -f *.o
	rm -f $(AOUT)
	rm -f $(OBJ)/*.mod
	rm -f travis/tests/*/*.out travis/tests/*/*.output travis/tests/*/instantaneousRates/*[0-9]


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
