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

# Script to generate the AtChem2 manual
#
# N.B.: the script MUST be run from the doc/ directory of AtChem2.

# Convert svg figures to png format using Inkscape (v1.x)
cd figures/
for FIG in *.svg; do
    inkscape --export-type="png" $FIG
done
cd ../

# Compile LaTeX source files to pdf file
cd latex/
pdflatex AtChem2-Manual.tex
bibtex AtChem2-Manual.aux
pdflatex AtChem2-Manual.tex
pdflatex AtChem2-Manual.tex

# Move pdf file to doc/ directory
cd ../
mv -f latex/AtChem2-Manual.pdf AtChem2-Manual.pdf

echo ""
echo "||----------------------------------------||"
echo "||==> AtChem2-Manual.pdf saved to doc/    ||"
echo "||----------------------------------------||"
echo ""
