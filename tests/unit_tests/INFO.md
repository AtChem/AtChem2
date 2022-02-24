The directory `tests/unit_tests/` contains the **unit tests** for
AtChem2. These tests check the output of individual subroutines and
functions of the model.

Modules tested:

- argparse.f90:
  - split_string
  - check_name_value_pair_validity

- atmosphereFunctions.f90:
  - calcAirDensity
  - calcAtmosphere
  - convertRHtoH2O
  - zero_equal (?)

- configFunctions.f90
  - matchNameToNumber
  - getIndexWithinList
  - findReactionsWithProductOrReactant
  - getSubsetOfConcs

- date_mod in dataStructures.f90:
  - isLeapYear
  - applyLeapDay
  - calcDayOfYear
  - calcInitialDateParameters
  - calcCurrentDateParameters

- solarFunctions.f90
  - calcTheta
  - decFromTheta
  - calcDec
  - calcEQT
