# -----------------------------------------------------------------------------
#
# Copyright (c) 2017-2025 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is licensed under the MIT license, which can be found in the file
# `LICENSE` at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# -------------------------------------------------------------------- #
# This script rewrites the Fortran source file provided as an argument,
# replacing it with a copy that better fits the expected style.
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
from __future__ import print_function
import sys
import re

# ============================================================ #


# Replace the first word(s) with its lowercase if it matches string
def replace_any_case_with_lower_first(string_in, string_out):
    if re.match(r"^\s*" + string_in, string_out, flags=re.IGNORECASE):
        string_out = re.sub(
            string_in, string_in.lower(), string_out, 1, flags=re.IGNORECASE
        )
    return string_out


# Replace the first word(s) with its lowercase if it matches string
def replace_any_case_with_lower(string_in, string_out):
    if re.search(string_in, string_out.upper(), flags=re.IGNORECASE):
        string_out = re.sub(
            string_in, string_in.lower(), string_out, 1, flags=re.IGNORECASE
        )
    return string_out


# Set first bracket to be preceded by no whitespace, followed by one space
def brackets_for_calls(
    string_in, string_out, first_of_multiline, in_procedure, on_multiline
):
    if re.match(r"^\s*" + string_in, string_out, flags=re.IGNORECASE):
        in_procedure = True
        string_out = replace_any_case_with_lower_first(string_in, string_out)
        if first_of_multiline or not on_multiline:
            if re.search(r"(?<=[a-zA-Z0-9\s])\(", string_out):
                string_out = re.sub(
                    r"\s*\(\s*(?=[a-zA-Z0-9\s\'\"\)])", r"( ", string_out, 1
                )
    return string_out, in_procedure, on_multiline


# Remove newline characters from string
def strip_newline(string_in):
    string_out = re.sub(r"\n", r"", string_in)
    return string_out


# Append newline character to string
def add_newline(string_in):
    string_out = string_in + "\n"
    return string_out


# Concatenate two strings
def add(string1, string2):
    string_out = add_newline(strip_newline(string1) + string2)
    return string_out


# Check that there are an even number of both single- and
# double-quotes in the given string
def even_quotes(string_in):
    if string_in.count('"') % 2 == 0:
        double = True
    else:
        double = False
    if string_in.count("'") % 2 == 0:
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
# Loop for fixing style
with open(out_filename, "w") as output_file:
    # Set up variable for the first time
    currently_on_multiline = False
    previous_line_ends_ampersand = False
    is_inside_procedure = False
    outputs = []
    for line in lines:
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

        # Book-keeping for matching bracketed calls over multiple lines
        this_line_ends_ampersand = False
        is_first_line_of_multiline = False

        # Handle the case where this line is split onto the next line(s)
        if re.search(r"\&\s*$", to_output):
            this_line_ends_ampersand = True
            if not currently_on_multiline:
                is_first_line_of_multiline = True
            currently_on_multiline = True

        # Replace '.LT.' etc with symbols
        to_output = re.sub(r"\s*\.LT\.\s*", r" < ", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\s*\.LE\.\s*", r" <= ", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\s*\.GT\.\s*", r" > ", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\s*\.GE\.\s*", r" >= ", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\s*\.EQ\.\s*", r" == ", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\s*\.NE\.\s*", r" /= ", to_output, flags=re.IGNORECASE)

        # Put one space after each comma, except where followed by '*' or ':'
        to_output = re.sub(r",\s*", r", ", to_output)
        to_output = re.sub(r", \*", r",*", to_output)
        to_output = re.sub(r", \:", r",:", to_output)

        # Replace, e.g. '(LEN ='  with '(len=', etc...
        to_output = re.sub(r"\(LEN\s*=", r"(len=", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\(KIND\s*=", r"(kind=", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"STATUS\s*=", r"status=", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"IOSTAT\s*=", r"iostat=", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"FILE\s*=", r"file=", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"EXIST\s*=", r"exist=", to_output, flags=re.IGNORECASE)

        # Any ending bracket followed by a double-quote, should have a
        # single space
        to_output = re.sub(r'\)(?=")', r")", to_output)

        # Any ending bracket should be followed by exactly one space
        # if it's to be followed by a letter, digit, or single-quote
        to_output = re.sub(r"\)(?=[\w\d])", r") ", to_output)

        # Any ending bracket already followed by whitespace should be
        # followed by exactly one space
        to_output = re.sub(r"\)[ \t]+", r") ", to_output)

        # These are math functions, so should be lower-case
        to_output = re.sub(r"ABS\(", r"abs(", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"LOG10\(", r"log10(", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"EXP\(", r"exp(", to_output, flags=re.IGNORECASE)

        # There are intrinsic functions, and should be lowercase
        to_output = re.sub(r"TRIM\(", r"trim(", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"ADJUSTL\(", r"adjustl(", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"ADJUSTR\(", r"adjustr(", to_output, flags=re.IGNORECASE)

        # A comma followed by any letter, digit, (, ', or -, should have a space trailing
        to_output = re.sub(r",(?=[a-zA-Z0-9('-])", r", ", to_output)

        # These are modifiers so should be lowercase
        to_output = re.sub(
            r",\s*ONLY\s*:\s*", r", only : ", to_output, flags=re.IGNORECASE
        )
        to_output = re.sub(
            r",\s*OPTIONAL\s*", r", optional", to_output, flags=re.IGNORECASE
        )
        to_output = re.sub(
            r",\s*CONTIGUOUS\s*", r", contiguous", to_output, flags=re.IGNORECASE
        )
        to_output = re.sub(
            r",\s*PARAMETER\s*", r", parameter", to_output, flags=re.IGNORECASE
        )
        to_output = re.sub(
            r",\s*PRIVATE\s*", r", private", to_output, flags=re.IGNORECASE
        )
        to_output = re.sub(
            r",\s*PUBLIC\s*", r", public", to_output, flags=re.IGNORECASE
        )

        # Place all '::' with exactly one space either side
        to_output = re.sub(r"\s*::\s*", r" :: ", to_output)

        # If it's a CALL, SUBROUTINE,  etc... line, then the first bracket is preceded by no
        # whitespace and the last bracket is preceded by one space, while handling the case
        # where this is split over multiple lines
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls(
            "CALL",
            to_output,
            is_first_line_of_multiline,
            is_inside_procedure,
            currently_on_multiline,
        )
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls(
            "SUBROUTINE",
            to_output,
            is_first_line_of_multiline,
            is_inside_procedure,
            currently_on_multiline,
        )
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls(
            "FUNCTION",
            to_output,
            is_first_line_of_multiline,
            is_inside_procedure,
            currently_on_multiline,
        )
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls(
            "PURE FUNCTION",
            to_output,
            is_first_line_of_multiline,
            is_inside_procedure,
            currently_on_multiline,
        )

        # Convert all these keywords, which start statements, to lowercase
        to_output = replace_any_case_with_lower_first("USE", to_output)
        to_output = replace_any_case_with_lower_first("IMPLICIT NONE", to_output)
        to_output = replace_any_case_with_lower_first("MODULE", to_output)
        to_output = replace_any_case_with_lower_first("CONTAINS", to_output)
        to_output = replace_any_case_with_lower_first("END SUBROUTINE", to_output)
        to_output = replace_any_case_with_lower_first("END FUNCTION", to_output)
        to_output = replace_any_case_with_lower_first("END MODULE", to_output)
        to_output = replace_any_case_with_lower_first("INTEGER", to_output)
        to_output = replace_any_case_with_lower_first("REAL", to_output)
        to_output = replace_any_case_with_lower_first("CHARACTER", to_output)
        to_output = replace_any_case_with_lower_first("LOGICAL", to_output)
        to_output = replace_any_case_with_lower_first("RETURN", to_output)
        to_output = replace_any_case_with_lower_first("DO", to_output)
        to_output = replace_any_case_with_lower_first("DO WHILE", to_output)
        to_output = replace_any_case_with_lower_first("END DO", to_output)
        to_output = replace_any_case_with_lower_first("ELSE", to_output)
        to_output = replace_any_case_with_lower_first("WRITE", to_output)
        to_output = replace_any_case_with_lower_first("READ", to_output)
        to_output = replace_any_case_with_lower_first("INQUIRE", to_output)
        to_output = replace_any_case_with_lower_first("PRINT", to_output)
        to_output = replace_any_case_with_lower_first("STOP", to_output)
        to_output = replace_any_case_with_lower_first("EXIT", to_output)
        to_output = replace_any_case_with_lower_first("OPEN", to_output)
        to_output = replace_any_case_with_lower_first("CLOSE", to_output)
        to_output = replace_any_case_with_lower_first("INCLUDE", to_output)
        to_output = replace_any_case_with_lower_first("IF", to_output)
        to_output = replace_any_case_with_lower_first("END IF", to_output)
        to_output = replace_any_case_with_lower_first("SELECT CASE", to_output)
        to_output = replace_any_case_with_lower_first("CASE", to_output)
        to_output = replace_any_case_with_lower_first("END SELECT", to_output)
        to_output = replace_any_case_with_lower_first("ALLOCATE", to_output)
        to_output = replace_any_case_with_lower_first("DEALLOCATE", to_output)
        to_output = replace_any_case_with_lower_first("DATA", to_output)
        to_output = replace_any_case_with_lower_first("PRIVATE", to_output)
        to_output = replace_any_case_with_lower_first("PUBLIC", to_output)

        # Split 'enddo' and 'endif' into two words, lowercase
        if re.match(r"\s*enddo", to_output, flags=re.IGNORECASE):
            to_output = re.sub(r"enddo", r"end do", to_output, 1, flags=re.IGNORECASE)
        if re.match(r"\s*endif", to_output, flags=re.IGNORECASE):
            to_output = re.sub(r"endif", r"end if", to_output, 1, flags=re.IGNORECASE)

        # These are all modifiers, so should be lowercase
        to_output = replace_any_case_with_lower("intent(in)", to_output)
        to_output = replace_any_case_with_lower("intent(inout)", to_output)
        to_output = replace_any_case_with_lower("intent(out)", to_output)
        to_output = replace_any_case_with_lower("allocatable", to_output)
        to_output = replace_any_case_with_lower("intrinsic", to_output)

        # Change boolean and relational operators to lowercase
        to_output = re.sub(r"\.true\.", r".true.", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\.false\.", r".false.", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\.eqv\.", r".eqv.", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\.not\.", r".not.", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\.or\.", r".or.", to_output, flags=re.IGNORECASE)
        to_output = re.sub(r"\.and\.", r".and.", to_output, flags=re.IGNORECASE)

        # If it's a INTEGER, REAL or CHARACTER line, then make the first
        # opening bracket be preceded by no whitespace, first closing
        # bracket to be preceded by one space unless there's a comma
        if (
            re.match(r"\s*integer\s*\(", to_output)
            or re.match(r"\s*real\s*\(", to_output)
            or re.match(r"\s*character\s*\(", to_output)
        ):
            to_output = re.sub(r"\s*\(", r"(", to_output, 1)
            to_output = re.sub(r"\)[^,]\s*", r") ", to_output, 1)

        # Match IF-THENs, give one space and change to lowercase
        if re.search(r"\s*IF.+THEN", to_output, flags=re.IGNORECASE):
            to_output = re.sub(r"IF\s*", r"if ", to_output, flags=re.IGNORECASE)
            to_output = re.sub(r"\s*THEN", r" then", to_output, flags=re.IGNORECASE)

        # Add space before last bracket of procedure call or
        # definition, handling multiple lines
        if is_inside_procedure and not this_line_ends_ampersand:
            if re.search(r"(?<=[a-zA-Z0-9])\)", to_output):
                to_output = re.sub(r"\s*\)(?=[^\)]*$)", r" )", to_output)
            is_inside_procedure = False

        # Change '( )' to '()'
        to_output = re.sub(r"\s*\(\s+\)\s*", r"()", to_output)

        # Change ')result' to ') result'
        if re.match(r"^\s*FUNCTION", to_output, flags=re.IGNORECASE) or re.match(
            r"^\s*PURE FUNCTION", to_output, flags=re.IGNORECASE
        ):
            to_output = re.sub(
                r"\)result \(", r") result (", to_output, flags=re.IGNORECASE
            )

        # End multiline environment if this line doesn't end with an ampersand
        if not this_line_ends_ampersand:
            currently_on_multiline = False

        # Add amended line to output
        outputs.append(to_output + add_newline(comment))

    # -------------------------------------------------
    # Write output to file
    output_file.writelines(outputs)
    print(
        'Complete! Now run a find and replace by hand with regex "[^\\n^  !]  " '
        "to catch incorrect multiple-spaces."
    )
