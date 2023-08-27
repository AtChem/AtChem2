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

# setup FRUIT
UNITTESTDIR = tests/unit_tests
fruit_code = $(FRUITDIR)/src/fruit.f90
unittest_code = $(UNITTEST_SRCS) $(shell ls tests/unit_tests/*_test.f90 )
unittest_code_gen = $(UNITTESTDIR)/fruit_basket_gen.f90 $(UNITTESTDIR)/fruit_driver_gen.f90
all_unittest_code = $(fruit_code) $(unittest_code) $(unittest_code_gen)
fruit_driver = $(UNITTESTDIR)/fruit_driver.exe

# copy fruit_generator.rb to the unit tests directory and replace the path of FRUIT with $(FRUITDIR)
$(UNITTESTDIR)/fruit_basket_gen.f90 : $(unittest_code)
	@echo ""
	@cp tests/fruit_generator.rb $(UNITTESTDIR)
	@cd $(UNITTESTDIR); sed -i "18s,.*,load \"$(FRUITDIR)/rake_base.rb\"," fruit_generator.rb; ruby fruit_generator.rb

# build fruit_driver.exe from the individual unit tests
$(fruit_driver) : $(all_unittest_code)
	$(FORT_COMP) -o $(fruit_driver) -J$(OBJ) -I$(OBJ) $(all_unittest_code) $(FFLAGS) $(LDFLAGS)

# search tests/tests/ for all subdirectories, which should reflect the full list of tests
OLDTESTS := $(shell ls -d tests/tests/*/ | sed 's,tests/tests/,,g' | sed 's,/,,g')

# search tests/model_tests/ for all subdirectories, which should reflect the full list of tests
MODELTESTSDIR = tests/model_tests
MODELTESTS := $(shell ls -d tests/model_tests/*/ | sed 's,tests/model_tests/,,g' | sed 's,/,,g')
