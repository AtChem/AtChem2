#!/usr/bin/python
##
## plotting tool for AtChem2 model output
##
## SCRIPT ARGUMENTS:
##   - model output directory
## ---------------------------------------------- ##

fin = open("envVar.output", "r")
fin = open("photolysisRates.output", "r")
fin = open("photoRateCalcParameters.output", "r")

x = fin.readline()
