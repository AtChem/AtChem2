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
else
# if osx, then pass self-built cvode and homebrew gfortran
CVODELIB=/Users/travis/build/AtChem/AtChem/cvode/lib
F77=/usr/local/Cellar/gcc@4.8/4.8.5/bin/gfortran-4.8
endif
# else it's not on Travis, so pass local path to cvode.
else
F77      =  gfortran
CVODELIB=/Users/sam/ReSET/Sommariva/cvode/lib
endif

# gfortran flags
FFLAGS   =  -ffree-form -fimplicit-none -Wall -Wpedantic
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

SRCS = dataStructures.f90 instantaneousRatesFunctions.f90 configFunctions.f90 inputFunctions.f90 outputFunctions.f90 atchem.f90 mechanism-rates.f90 solverFunctions.f90 interpolationFunctions.f90 constraintFunctions.f90 conversionFunctions.f90 utilityFunctions.f90

LDFLAGS = -L$(CVODELIB) -Wl,-rpath,$(LIBDIR) -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial -lblas -llapack

# prerequisite is $(SRCS), so this will be rebuilt everytime any source file in $(SRCS) changes
$(AOUT): $(SRCS)
	$(F77) -o $(AOUT) $(SRCS) $(FFLAGS) $(LDFLAGS)
	@perl -ne 'm/\d+\.\d*[eE][-+]?\d+/ and push @a, "$$ARGV:$$.: $$&:\t$$_";END{@a and print("\nWARNING! Single-precision constants found:\n", @a)}' *.f90

TESTS := short single_reac_of_interest short_extended full spec_no_env_yes1 spec_yes_env_no spec_yes_env_yes

test:
	@echo "Make: performing 'make clean'."
	@make clean
	@echo "Make: Running the following tests:" $(TESTS)
	@rm -f travis/tests/results
	@./travis/test_runner.sh "$(TESTS)" "$(CVODELIB)"

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
conversionFunctions.o : conversionFunctions.f90
dataStructures.o : dataStructures.f90
inputFunctions.o : inputFunctions.f90 configFunctions.o dataStructures.o
instantaneousRatesFunctions.o : instantaneousRatesFunctions.f90
interpolationFunctions.o : interpolationFunctions.f90 dataStructures.o
mechanism-rates.o : mechanism-rates.f90 dataStructures.o
configFunctions.o : configFunctions.f90
outputFunctions.o : outputFunctions.f90 dataStructures.o
solverFunctions.o : solverFunctions.f90 dataStructures.o
