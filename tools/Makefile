# -----------------------------------------------------------------------------
#
# Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
# Peter Jimack, Mike Pilling
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

#!/bin/sh
# Makefile for AtChem2

.SUFFIXES:
.SUFFIXES: .f90 .o
.PHONY: all test

OS := $(shell uname -s)
CVODELIB     = cvode/lib
OPENLIBMDIR  = openlibm-0.4.1
FRUITDIR     = fruit_3.4.3

ifeq ($(TRAVIS),true)
ifeq ($(TRAVIS_OS_NAME),linux)
# if linux, pass gfortran
FORT_COMP    = gfortran
FORT_LIB     = ""
else
# if macOS, pass homebrew gfortran
FORT_COMP    = /usr/local/Cellar/gcc@4.9/4.9.4_1/bin/gfortran-4.9
FORT_LIB     = /usr/local/Cellar/gcc@4.9/4.9.4_1/lib/gcc/4.9
endif
# else it's not on Travis, so set the fortran compiler as gfortran
else
FORT_COMP    = gfortran
FORT_LIB     = ""
endif

ifeq ($(OS),Linux)
RPATH_OPTION = -R
else
RPATH_OPTION = -rpath
endif

# GNU gfortran flags
FFLAGS   =  -fprofile-arcs -ftest-coverage -ffree-form -fimplicit-none -Wall -Wpedantic -fcheck=all
LIBDIR   =  /usr/lib/:$(CVODELIB)

# Intel ifort flags
#FORT_COMP = ifort
#FFLAGS   = -free -warn
#LIBDIR =

OBJ = obj
SRC = src
UNITTESTDIR = travis/unit_tests

AOUT = atchem2

all: $(AOUT)

UNITTEST_SRCS = $(SRC)/dataStructures.f90 $(SRC)/interpolationFunctions.f90 $(SRC)/configFunctions.f90 $(SRC)/inputFunctions.f90 $(SRC)/outputFunctions.f90 $(SRC)/atmosphereFunctions.f90 $(SRC)/solarFunctions.f90 $(SRC)/constraintFunctions.f90 $(SRC)/solverFunctions.f90 $(SRC)/parameterModules.f90
SRCS = $(UNITTEST_SRCS) $(SRC)/atchem2.f90

LDFLAGS = -L$(CVODELIB) -L$(OPENLIBMDIR) -Wl,$(RPATH_OPTION),$(LIBDIR):$(OPENLIBMDIR) -lopenlibm -lsundials_fcvode -lsundials_cvode -lsundials_fnvecserial -lsundials_nvecserial -ldl

# prerequisite is $(SRCS), so this will be rebuilt everytime any source file in $(SRCS) changes
$(AOUT): sharedlib $(SRCS)
	$(FORT_COMP) -o $(AOUT) -J$(OBJ) -I$(OBJ) $(SRCS) $(FFLAGS) $(LDFLAGS)

fruit_code = $(FRUITDIR)/src/fruit.f90
unittest_code = $(UNITTEST_SRCS) $(shell ls travis/unit_tests/*_test.f90 )
unittest_code_gen = $(UNITTESTDIR)/fruit_basket_gen.f90 $(UNITTESTDIR)/fruit_driver_gen.f90
all_unittest_code = $(fruit_code) $(unittest_code) $(unittest_code_gen)
fruit_driver = $(UNITTESTDIR)/fruit_driver.exe

# copy fruit_generator.rb to the unit tests directory and replace the path of FRUIT with $(FRUITDIR)
$(UNITTESTDIR)/fruit_basket_gen.f90 : $(unittest_code)
	@cp tools/fruit_generator.rb $(UNITTESTDIR)
	@cd $(UNITTESTDIR); sed -i "4s,.*,load \"$(FRUITDIR)/rake_base.rb\"," fruit_generator.rb; ruby fruit_generator.rb

$(fruit_driver) : $(all_unittest_code)
	$(FORT_COMP) -o $(fruit_driver) -J$(OBJ) -I$(OBJ) $(all_unittest_code) $(FFLAGS) $(LDFLAGS)

unittests: $(fruit_driver)
	@export DYLD_LIBRARY_PATH=$(FORT_LIB):$(CVODELIB):$(OPENLIBMDIR) ; $(fruit_driver)

# search travis/tests/ for all subdirectories, which should reflect the full list of tests
TESTS := $(shell ls -d travis/tests/*/ | sed 's,travis/tests/,,g' | sed 's,/,,g')

tests:
	@echo "Make: Running the following tests:" $(TESTS)
	@./travis/run_tests.sh "$(TESTS)" "$(FORT_LIB):$(CVODELIB):$(OPENLIBMDIR)"

formattest:
	@./travis/run_format_test.sh

styletest:
	@./travis/run_style_test.sh

alltests: formattest styletest unittests tests

sharedlib:
	$(FORT_COMP) -c src/gen/mechanism-rate-coefficients.f90
	$(FORT_COMP) -shared -o mechanism-rate-coefficients.so mechanism-rate-coefficients.o

clean:
	rm -f *.o
	rm -f *.gcda *.gcno *.xml
	rm -f $(AOUT)
	rm -f $(OBJ)/*.mod
	rm -f travis/tests/*/*.out travis/tests/*/*.output travis/tests/*/instantaneousRates/*[0-9]
	rm -f $(UNITTESTDIR)/fruit_basket_gen.f90 $(UNITTESTDIR)/fruit_driver_gen.f90 $(fruit_driver)

# dependencies:
atchem2.o : atchem2.f90 inputFunctions.o configFunctions.o dataStructures.o
constraintFunctions.o : constraintFunctions.f90 dataStructures.o
atmosphereFunctions.o : atmosphereFunctions.f90
dataStructures.o : dataStructures.f90
inputFunctions.o : inputFunctions.f90 configFunctions.o dataStructures.o
interpolationFunctions.o : interpolationFunctions.f90 dataStructures.o
configFunctions.o : configFunctions.f90
outputFunctions.o : outputFunctions.f90 dataStructures.o
parameterModules.o : parameterModules.f90 dataStructures.o
solverFunctions.o : solverFunctions.f90 dataStructures.o
