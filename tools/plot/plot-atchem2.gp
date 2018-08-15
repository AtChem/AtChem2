# -----------------------------------------------------------------------------
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

## plotting tool for the AtChem2 model output
## --> gnuplot version
##
## ARGUMENT:
## - directory with the model output (default = modelOutput/)
##
## USAGE:
##   gnuplot -c plot-atchem2.gp modelOutput/
## ---------------------------------------------- ##
cd ARG1
pwd

df1 = 'speciesConcentrations.output'
df2 = 'environmentVariables.output'
df3 = 'photolysisRates.output'
df4 = 'photolysisRatesParameters.output'

stats df1 skip 1 noout; nc1 = STATS_columns
stats df2 skip 1 noout; nc2 = STATS_columns
stats df3 skip 1 noout; nc3 = STATS_columns
stats df4 skip 1 noout; nc4 = STATS_columns

## ---------------------------- ##

set terminal pdf size 11,7
set output 'atchem2_output.pdf'

## speciesConcentrations.output
set multiplot layout 3,2
j = 1
do for [i=2:nc1] {
    plot df1 using 1:i with lines title '' lt rgb 'black'
    set title ''; set xlabel 'seconds'; set ylabel ''
    if (j == 6) {
        unset multiplot
        set multiplot layout 3,2
        j = 1
    } else {
        j = j + 1
    }
}
unset multiplot

## environmentVariables.output
set multiplot layout 3,2
j = 1
do for [i=2:nc2] {
    plot df2 using 1:i with lines title '' lt rgb 'black'
    set title ''; set xlabel 'seconds'; set ylabel ''
    if (j == 6) {
        unset multiplot
        set multiplot layout 3,2
        j = 1
    } else {
        j = j + 1
    }
}
unset multiplot

## photolysisRates.output
set multiplot layout 3,2
j = 1
do for [i=2:nc3] {
    plot df3 using 1:i with lines title '' lt rgb 'black'
    set title ''; set xlabel 'seconds'; set ylabel ''
    if (j == 6) {
        unset multiplot
        set multiplot layout 3,2
        j = 1
    } else {
        j = j + 1
    }
}
unset multiplot

## photolysisRatesParameters.output
set multiplot layout 3,2
j = 1
do for [i=2:nc4] {
    plot df4 using 1:i with lines title '' lt rgb 'black'
    set title ''; set xlabel 'seconds'; set ylabel ''
    if (j == 6) {
        unset multiplot
        set multiplot layout 3,2
        j = 1
    } else {
        j = j + 1
    }
}
unset multiplot

## ---------------------------- ##

print "\n===> atchem2_output.pdf created in directory: ", ARG1, "\n\n"
