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

# Script to generate the AtChem2 manual pdf file.
#
# N.B.: the script MUST be run from the main directory of AtChem2.

# Convert svg figures to png format using Inkscape (v1.x)
cd doc/figures/
for FIG in *.svg; do
    inkscape --export-type="png" --export-filename=${FIG%%.*}.png $FIG
done

# Compile LaTeX source files, generate pdf file
cd ../latex/
pdflatex AtChem2-Manual.tex
bibtex AtChem2-Manual.aux
pdflatex AtChem2-Manual.tex
pdflatex AtChem2-Manual.tex

# Move pdf file to doc/ directory
#mv -f AtChem2-Manual.pdf ../AtChem2-Manual.pdf

echo ""
echo "||----------------------------------------||"
echo "||    AtChem2-Manual.pdf saved to doc/    ||"
echo "||----------------------------------------||"
echo ""

exit 0
