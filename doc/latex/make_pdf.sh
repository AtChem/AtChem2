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

# Compile the LaTeX source files of the AtChem2 manual into a pdf file

pdflatex AtChem2-Manual.tex
pdflatex AtChem2-Manual.tex

mv -f AtChem2-Manual.pdf ../AtChem2-Manual.pdf
