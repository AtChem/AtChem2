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
## --> Python version [requires matplotlib]
##
## ARGUMENT:
## - directory with the model output (default = modelOutput/)
##
## USAGE:
##   python plot-atchem2.py modelOutput/
## ---------------------------------------------- ##
import os, sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

os.chdir(sys.argv[1])
print os.getcwd()

with open('speciesConcentrations.output') as f:
    var1 = f.readline().split()
with open('environmentVariables.output') as f:
    var2 = f.readline().split()
with open('photolysisRates.output') as f:
    var3 = f.readline().split()
with open('photolysisRatesParameters.output') as f:
    var4 = f.readline().split()

df1 = np.loadtxt('speciesConcentrations.output', skiprows=1, unpack=True)
df2 = np.loadtxt('environmentVariables.output', skiprows=1, unpack=True)
df3 = np.loadtxt('photolysisRates.output', skiprows=1, unpack=True)
df4 = np.loadtxt('photolysisRatesParameters.output', skiprows=1, unpack=True)

nc1 = df1.shape[0]
nc2 = df2.shape[0]
nc3 = df3.shape[0]
nc4 = df4.shape[0]

## ---------------------------- ##

with PdfPages('atchem2_output.pdf') as pdf:

    ## speciesConcentrations.output
    fig = plt.figure(figsize=(11,7))
    j = 1
    for i in range(1,nc1):
        ax = fig.add_subplot(3,2,j)
        ax.plot(df1[0], df1[i], linestyle='-', color='black')
        ax.set(title=var1[i], xlabel='seconds', ylabel='')
        if j == 6:
            pdf.savefig(fig)
            fig = plt.figure(figsize=(11,7))
            j = 1
        else:
            j = j + 1
    pdf.savefig(fig)

    ## environmentVariables.output
    fig = plt.figure(figsize=(11,7))
    j = 1
    for i in range(1,nc2):
        ax = fig.add_subplot(3,2,j)
        ax.plot(df2[0], df2[i], linestyle='-', color='black')
        ax.set(title=var2[i], xlabel='seconds', ylabel='')
        if j == 6:
            pdf.savefig(fig)
            fig = plt.figure(figsize=(11,7))
            j = 1
        else:
            j = j + 1
    pdf.savefig(fig)

    ## photolysisRates.output
    fig = plt.figure(figsize=(11,7))
    j = 1
    for i in range(1,nc3):
        ax = fig.add_subplot(3,2,j)
        ax.plot(df3[0], df3[i], linestyle='-', color='black')
        ax.set(title=var3[i], xlabel='seconds', ylabel='')
        if j == 6:
            pdf.savefig(fig)
            fig = plt.figure(figsize=(11,7))
            j = 1
        else:
            j = j + 1
    pdf.savefig(fig)

    ## photolysisRatesParameters.output
    fig = plt.figure(figsize=(11,7))
    j = 1
    for i in range(1,nc4):
        ax = fig.add_subplot(3,2,j)
        ax.plot(df4[0], df4[i], linestyle='-', color='black')
        ax.set(title=var4[i], xlabel='seconds', ylabel='')
        if j == 6:
            pdf.savefig(fig)
            fig = plt.figure(figsize=(11,7))
            j = 1
        else:
            j = j + 1
    pdf.savefig(fig)

## ---------------------------- ##

print "\n===> atchem2_output.pdf created in directory:", sys.argv[1], "\n\n"
