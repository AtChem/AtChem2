# -----------------------------------------------------------------------------
#
# Copyright (c) 2017 Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

## Python plotting tool for AtChem2 model output
##
## SCRIPT ARGUMENT:
##   - model output directory
## ---------------------------------------------- ##

fin = open("envVar.output", "r")
fin = open("photolysisRates.output", "r")
fin = open("photoRateCalcParameters.output", "r")

x = fin.readline()
