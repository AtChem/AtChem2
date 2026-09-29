# -----------------------------------------------------------------------------
#
# Copyright (c) 2009-2012 Chris Martin, Kasia Boronska, Jenny Young,
# Peter Jimack, Mike Pilling
#
# Copyright (c) 2017-2027 Sam Cox, Roberto Sommariva, James Allsopp
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# This is a secondary Makefile, which defines the rules for the AtChem2
# Testsuite.
#
# This Makefile should not be run directly with `make -f`, as it is
# included by the main Makefile.
#
# The following variables are defined in the main Makefile:
# - $FRUITDIR: path to FRUIT, the FORTRAN Unit Test Framework
# - $CORE_SRCS: Fortran source files needed for the unit tests
# - $FORT_COMP: Fortran compiler
# - $CVODELIBDIR, $OPENLIBMDIR: paths to dependencies (CVODE, openlibm)

ifndef FORT_COMP
  $(error )
endif

# ==================== Unit tests ==================== #

# setup FRUIT
UNITTESTDIR = tests/unit_tests
fruit_code = $(FRUITDIR)/src/fruit.f90
unittest_code = $(CORE_SRCS) $(shell ls tests/unit_tests/*_test.f90 )
unittest_code_gen = $(UNITTESTDIR)/fruit_basket_gen.f90 $(UNITTESTDIR)/fruit_driver_gen.f90
all_unittest_code = $(fruit_code) $(unittest_code) $(unittest_code_gen)
fruit_driver = $(UNITTESTDIR)/fruit_driver.exe

# copy `fruit_generator.rb` to the unit tests directory and replace the
# path to FRUIT with $FRUITDIR
$(UNITTESTDIR)/fruit_basket_gen.f90 : $(unittest_code)
	@echo ""
	@cp tests/fruit_generator.rb $(UNITTESTDIR)
	@cd $(UNITTESTDIR); \
	sed -i "18s,.*,load \"$(FRUITDIR)/rake_base.rb\"," fruit_generator.rb; \
	ruby fruit_generator.rb

# build `fruit_driver.exe` from the individual unit tests
$(fruit_driver) : $(all_unittest_code)
	$(FORT_COMP) -o $(fruit_driver) -J$(OBJ) -I$(OBJ) $(all_unittest_code) $(FFLAGS) $(LDFLAGS)

# ==================== Model tests  ==================== #

# search `tests/tests/` for all subdirectories, which should reflect the full list of tests
OLDTESTS := $(shell ls -d tests/tests/*/ | sed 's,tests/tests/,,g' | sed 's,/,,g')

# search `tests/model_tests/` for all subdirectories, which should reflect the full list of model tests
MODELTESTDIR = tests/model_tests
MODELTESTS := $(shell ls -d tests/model_tests/*/ | sed 's,tests/model_tests/,,g' | sed 's,/,,g')

# ==================== Makefile rules  ==================== #

.PHONY: indenttest styletest pythontest unittests oldtests modeltests alltests cleantests

indenttest:
	@echo ""
	@echo "Make: check the indentation of the Fortran source code."
	@./tests/run_indent_test.sh

styletest:
	@echo ""
	@echo "Make: check the style of the Fortran source code."
	@./tests/run_style_test.sh

pythontest:
	@echo ""
	@echo "Make: check the format of the Python scripts."
	@./tests/python_format_test.sh

unittests: $(fruit_driver)
	@export DYLD_LIBRARY_PATH=$(FORT_LIB):$(CVODELIBDIR):$(OPENLIBMDIR); $(fruit_driver)
	@echo ""

oldtests:
	@echo ""
	@echo "Make: Running the old tests:" $(OLDTESTS)
	@./tests/run_tests.sh "$(OLDTESTS)" "$(FORT_LIB):$(CVODELIBDIR):$(OPENLIBMDIR)"

modeltests:
	@echo ""
	@echo "Make: Running the model tests:" $(MODELTESTS)
	@./tests/run_model_tests.sh "$(MODELTESTS)" "$(FORT_LIB):$(CVODELIBDIR):$(OPENLIBMDIR)"

alltests: indenttest styletest pythontest unittests oldtests modeltests

cleantests:
	rm -f tests/tests/*/*.out tests/tests/*/model/configuration/include/mechanism.{f90,prod,reac,ro2,species} \
              tests/tests/*/model/configuration/include/*.{o,mod,so} \
              tests/tests/*/output/*.output tests/testsc/*/output/reactionRates/*[0-9]
	rm -f $(MODELTESTDIR)/*/*.out $(MODELTESTDIR)/*/configuration/include/mechanism.{f90,prod,reac,ro2,species} \
              $(MODELTESTDIR)/*/configuration/include/*.{o,mod,so} \
              $(MODELTESTDIR)/*/output/*.output $(MODELTESTDIR)/*/output/reactionRates/*[0-9]
	rm -f $(UNITTESTDIR)/fruit_*_gen.f90 $(UNITTESTDIR)/fruit_generator.rb $(fruit_driver)
