#!/bin/sh
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

# ------------------------------------------------------------------ #
# Script to generate the PDF file of the AtChem2 manual.
#
# NB: the script MUST be run from the Main Directory of AtChem2.
# ------------------------------------------------------------------ #

# Function to run a command and check whether it is successful
run() {
    "$@"
    status=$?
    if [ "$status" -ne 0 ]; then
        printf "\n[FAIL] $*\n"
        exit "$status"
    else
        printf "\n[PASS] $*\n"
    fi
}

# ------------------------------------------------------------------ #

# Convert figures from `.svg` to `.png` format, using ImageMagick (v7.x)
cd doc/figures/
for FIG in *.svg; do
    run magick "$FIG" "${FIG%%.*}.png"
done

# Compile LaTeX source files and generate PDF file
cd ../latex/
run pdflatex AtChem2-Manual.tex
run bibtex AtChem2-Manual.aux
run pdflatex AtChem2-Manual.tex
run pdflatex AtChem2-Manual.tex

# Move PDF file to doc/ directory
run mv -f AtChem2-Manual.pdf ../AtChem2-Manual.pdf

printf "\n||----------------------------------------||"
printf "\n||    AtChem2-Manual.pdf saved to doc/    ||"
printf "\n||----------------------------------------||\n"

exit 0
