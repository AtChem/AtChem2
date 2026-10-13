#!/bin/sh
# -----------------------------------------------------------------------------
#
# Copyright (c) 2017-2027 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# ------------------------------------------------------------------ #
# Script to generate the PDF file of the AtChem2 manual.
#
# Requirements: TeXLive, ImageMagick (v7.x)
#
# Usage:
#   ./doc/make_manual_pdf.sh
# ------------------------------------------------------------------ #
set -eu

# function to run a command and check whether it is successful
run() {
    if "$@"; then
        printf "\n[SUCCESS] %s\n" "$*"
    else
        status=$?
        printf "\n[FAIL] %s\n" "$*"
        exit "$status"
    fi
}

# ============================================================ #
cd "$(dirname "$0")"

# convert figures from `.svg` to `.png` format
cd figures/
for FIG in *.svg; do
    run magick "$FIG" "${FIG%%.*}.png"
done

# compile LaTeX source files and generate PDF file
cd ../latex/
run pdflatex AtChem2-Manual.tex
run bibtex AtChem2-Manual.aux
run pdflatex AtChem2-Manual.tex
run pdflatex AtChem2-Manual.tex

# move PDF file to doc/ directory
run mv -f AtChem2-Manual.pdf ../AtChem2-Manual.pdf

printf "\n||----------------------------------------||"
printf "\n||    AtChem2-Manual.pdf saved to doc/    ||"
printf "\n||----------------------------------------||\n\n"
exit 0
