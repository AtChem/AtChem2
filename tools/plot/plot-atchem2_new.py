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
## --> Python2 version [requires pandas & matplotlib]
##
## ARGUMENT:
## - directory with the model output
##
## USAGE:
##   python ./tools/plot/plot-atchem2.py ./model/output/
## ---------------------------------------------- ##
import os, sys
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

def parse_file(input_file, output_file, rows=3, columns=2):
    df = pd.read_csv(input_file, sep="\s+")
    figures = generate_plots(df, rows, columns)
    save_plots(figures, output_file)

def generate_plots(df, rows=3, columns=2):
    for group in column_grouper(df, rows*columns):
        fig = plt.figure(figsize=(11, 7))
        for i, (label, column) in enumerate(group.iteritems()):
            ax = fig.add_subplot(rows, columns, i+1)
            ax.plot(df['t'], column, linestyle='-', color='black')
            ax.set(title=label, xlabel='seconds', ylabel='')
        yield fig

def column_grouper(df, n):
    for i in range(1, df.shape[1], n):
        yield df.iloc[:, i:i+n]

def save_plots(figures, output_file):
    for fig in figures:
        pdf.savefig(fig)

## ---------------------------- ##

if __name__ == "__main__":
    input_files = ['speciesConcentrations.output', 'environmentVariables.output',
                   'photolysisRates.output', 'photolysisRatesParameters.output']
    output_file = 'atchem2_output.pdf'

    os.chdir(sys.argv[1])
    print os.getcwd()

    with PdfPages(output_file) as pdf:
        for input_file in input_files:
            parse_file(input_file, output_file)

## ---------------------------- ##

print "\n===> atchem2_output.pdf created in directory:", sys.argv[1], "\n\n"
