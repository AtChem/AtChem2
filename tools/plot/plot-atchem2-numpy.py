# -----------------------------------------------------------------------------
#
# Copyright (c) 2017-2025 Sam Cox, Roberto Sommariva, Marios Panagi
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

## Plotting tool for the AtChem2 model output
## --> version for Python [requires numpy & matplotlib]
##
## ARGUMENT:
## - directory with the model output
##
## USAGE:
##   python ./tools/plot/plot-atchem2-numpy.py ./model/output/
## ---------------------------------------------- ##
from __future__ import print_function
import os, sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

if len(sys.argv) < 2:
    print("[!] Please provide the model output directory as an argument.")
    exit()
else:
    output_dir = sys.argv[1]
    os.chdir(output_dir)

var1 = np.genfromtxt("speciesConcentrations.output", max_rows=1, dtype=str)
df1 = np.genfromtxt("speciesConcentrations.output", skip_header=1)

var2 = np.genfromtxt("environmentVariables.output", max_rows=1, dtype=str)
df2 = np.genfromtxt("environmentVariables.output", skip_header=1)

var3 = np.genfromtxt("photolysisRates.output", max_rows=1, dtype=str)
df3 = np.genfromtxt("photolysisRates.output", skip_header=1)

var4 = np.genfromtxt("photolysisRatesParameters.output", max_rows=1, dtype=str)
df4 = np.genfromtxt("photolysisRatesParameters.output", skip_header=1)

nc1 = df1.shape[1]
nc2 = df2.shape[1]
nc3 = df3.shape[1]
nc4 = df4.shape[1]

## ---------------------------- ##

with PdfPages("atchem2_output.pdf") as pdf:

    ## speciesConcentrations.output
    fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
    axs = axs.ravel()
    j = 0
    for i in range(1, nc1):
        ax = axs[j]
        ax.plot(df1[:, 0], df1[:, i], linestyle="-", color="black")
        ax.set(title=var1[i], xlabel="seconds", ylabel="")
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: "%.1e" % x))
        plt.tight_layout()
        if j == 8:
            pdf.savefig(fig)
            fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
            axs = axs.ravel()
            j = 0
        else:
            j = j + 1
    pdf.savefig(fig)

    ## environmentVariables.output
    fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
    axs = axs.ravel()
    j = 0
    for i in range(1, nc2):
        ax = axs[j]
        ax.plot(df2[:, 0], df2[:, i], linestyle="-", color="black")
        ax.set(title=var2[i], xlabel="seconds", ylabel="")
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: "%.1e" % x))
        plt.tight_layout()
        if j == 8:
            pdf.savefig(fig)
            fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
            axs = axs.ravel()
            j = 0
        else:
            j = j + 1
    pdf.savefig(fig)

    ## photolysisRates.output
    fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
    axs = axs.ravel()
    j = 0
    for i in range(1, nc3):
        ax = axs[j]
        ax.plot(df3[:, 0], df3[:, i], linestyle="-", color="black")
        ax.set(title=var3[i], xlabel="seconds", ylabel="")
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: "%.1e" % x))
        plt.tight_layout()
        if j == 8:
            pdf.savefig(fig)
            fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
            axs = axs.ravel()
            j = 0
        else:
            j = j + 1
    pdf.savefig(fig)

    ## photolysisRatesParameters.output
    fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
    axs = axs.ravel()
    j = 0
    for i in range(1, nc4):
        ax = axs[j]
        ax.plot(df4[:, 0], df4[:, i], linestyle="-", color="black")
        ax.set(title=var4[i], xlabel="seconds", ylabel="")
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: "%.1e" % x))
        plt.tight_layout()
        if j == 8:
            pdf.savefig(fig)
            fig, axs = plt.subplots(nrows=3, ncols=3, figsize=(11, 7))
            axs = axs.ravel()
            j = 0
        else:
            j = j + 1
    pdf.savefig(fig)

## ---------------------------- ##

print("\n==> atchem2_output.pdf created in directory:", output_dir, "\n")
