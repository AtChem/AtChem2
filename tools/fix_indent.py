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

# -------------------------------------------------------------------- #
# This script rewrites the Fortran source file provided as an argument,
# replacing it with a copy that better fits the expected style (with
# regard to code indentation).
# If two arguments are given, the output will be written to the second,
# leaving the first untouched.
#
# WARNING: this script is not infallible, and can break your code!
# Please use with caution, and make sure you have a copy of your
# source file to revert to in the event of it breaking.
#
# ARGUMENT(S):
#   1. path to the fortran file to process
#   2. optional output file (if not given, overwrites input file)
# -------------------------------------------------------------------- #
# The workflow of this script is straightforward. For each line in the
# Fortran source file:
#  * strip off the trailing newline
#  * strip out the comment from the end of the line, relying on the fact that
#    quotes cannot span lines, to ensure we really grab a '!' marking the
#    comment start, and not one inside a string literal
#  * if the non-comment content is all whitespace, then skip to reappending the
#    comment, otherwise:
#    * if the previous line was e.g. the start of a procedure, then increase the
#      indent on this line
#    * store whether this line ends in an ampersand
#    * if the previous line ended in an ampersand, then we don't know where to
#      align to, so leave untouched
#    * otherwise, the presence of 'end', 'else' or 'contains' means that this
#      line should be unindented
#    * if this line is the start of e.g. a procedure, store that the next line
#      (that isn't following an ampersand) should be indented
#  * perform appropriate indentation, rejoin comment and newline, and output
#  * set up variables for next loop, passing on indent setting and whether this
#    line ended with an ampersand
# -------------------------------------------------------------------- #
from __future__ import print_function
import sys
import re

# ============================================================ #


# Strip newline characters from string
def strip_newline(string):
    string = re.sub(r"\n", r"", string)
    return string


# Append newline character to string
def add_newline(string):
    string = string + "\n"
    return string


# Check that there are an even number of both single- and
# double-quotes in the given string
def even_quotes(string):
    if string.count('"') % 2 == 0:
        double = True
    else:
        double = False
    if string.count("'") % 2 == 0:
        single = True
    else:
        single = False
    return double and single


# ============================================================ #

# Handle input arguments. If only one is provided, use this for both
# input and output. Error if none provided.
assert len(sys.argv) >= 2, "Please enter a filename as argument."
in_filename = sys.argv[1]
if len(sys.argv) == 3:
    out_filename = sys.argv[2]
else:
    out_filename = in_filename

# Read in file contents
with open(in_filename, "r") as input_file:
    lines = input_file.readlines()

# -------------------------------------------------
# Loop for fixing indent
with open(out_filename, "w") as output_file:
    # Set up variables for first time
    previous_line_ends_ampersand = False
    this_line_ends_ampersand = False
    next_line_indent_more = False
    empty_line = False
    start_select = False
    indent = 0
    outputs = []
    for line in lines:
        empty_line = False
        # Extract comment from end of line, to append later
        line = strip_newline(line)
        split_line = line.split("!")
        to_output = split_line[0]
        if len(split_line) > 1:
            # Cycle over each combo of strings until we split command from comment
            for i in range(len(split_line)):
                to_output = "!".join(split_line[0 : i + 1])
                if even_quotes(to_output):
                    if len(split_line) >= i + 2 and not split_line[i + 1].isspace():
                        comment = strip_newline("!" + "!".join(split_line[i + 1 :]))
                    else:
                        comment = ""
                    break
        else:
            comment = ""

        # Check that the string contains only whitespace or is empty
        if to_output.isspace() or to_output == "":
            empty_line = True

        # Keep track of the indentation level
        if next_line_indent_more:
            indent = indent + 1
            next_line_indent_more = False

        # Check that this line ends with ampersand
        if not empty_line:
            this_line_ends_ampersand = False
            if re.search(r"\&\s*$", to_output):
                this_line_ends_ampersand = True

            # This line starts with 'end', so the next line should be unindented
            if not previous_line_ends_ampersand:
                if (
                    re.match(r"^\s*end\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*else\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*contains\s*", to_output, flags=re.IGNORECASE)
                ):
                    indent = indent - 1

                # Handle the fact that each case of a 'select' structure doesn't
                # end in an 'end', so the 'end select' needs to go back twice
                if re.match(r"^\s*end select\s*", to_output, flags=re.IGNORECASE):
                    indent = indent - 1

                # Match 'if-then-else', 'do', 'subroutine', 'function', 'module', 'contains',
                # 'program', 'interface', 'select', 'case', 'type' (definition, not instantiation)
                # to set the next line indent higher
                if (
                    re.search(r"\s*if.+then", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*else\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*do\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*subroutine\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*function\s*", to_output, flags=re.IGNORECASE)
                    or (
                        re.match(r"^\s*module\s*", to_output, flags=re.IGNORECASE)
                        and not re.match(
                            r"^\s*module procedure \s*", to_output, flags=re.IGNORECASE
                        )
                    )
                    or re.match(r"^\s*contains\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*program\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*interface\s*", to_output, flags=re.IGNORECASE)
                    or re.match(
                        r"^\s*abstract interface\s*", to_output, flags=re.IGNORECASE
                    )
                    or re.match(r"^\s*pure function\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*select\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*case\s*", to_output, flags=re.IGNORECASE)
                    or re.match(r"^\s*type\s+", to_output, flags=re.IGNORECASE)
                ):
                    next_line_indent_more = True

                # Set start_select when we enter a 'select' structure
                if re.match(r"^\s*select\s*", to_output, flags=re.IGNORECASE):
                    start_select = True

                # If at a 'case' statement, check whether it's the first one, via start_select.
                # If so, don't change the indent, as we just want it to be indented next time;
                # otherwise, unindent by one
                if re.match(r"^\s*case\s*", to_output, flags=re.IGNORECASE):
                    if start_select:
                        start_select = False
                    else:
                        indent = indent - 1

            # This line does not start with 'end'
            else:
                next_line_indent_more = False

        # Check that the previous line does not end with ampersand,
        # then add correct indentation
        if not previous_line_ends_ampersand:
            if re.search(r"\S", to_output):
                to_output = re.sub(r"^\s*(?=\S)", r" " * 2 * indent, to_output)
            elif re.search(r"\S", to_output + comment):
                to_output = ""
                comment = (" " * 2 * indent) + comment

        # Update previous_line_ends_ampersand, based on whether this
        # line ends with an ampersand
        if this_line_ends_ampersand:
            previous_line_ends_ampersand = True
        else:
            previous_line_ends_ampersand = False

        # Add amended line to output
        outputs.append(to_output + add_newline(comment))

    # -------------------------------------------------
    # Write output to file
    output_file.writelines(outputs)
    print(
        'Complete! Now run a find and replace by hand with regex "\&\s*\\n" '
        "to catch alignment of the lines following ampersands."
    )
