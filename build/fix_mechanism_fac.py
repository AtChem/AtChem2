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

# This script corrects the content of a chemical mechanism file in
# FACSIMILE format (.fac ) by removing incorrect newline characters.
#
# Due to a bug of the extraction tool on the MCM website, sometimes
# the newline character is inserted in the wrong place. For example:
#
#   % 3.8D-13*EXP(780/TEMP)*(1-1/(1+498*EXP(-1160/TEMP))) : CH3O2 + HO2 =
#   CH3OOH ; % 3.8D-13*EXP(780/TEMP)*(1/(1+498*EXP(-1160/TEMP))) : CH3O2 + HO2 =
#   HCHO ;
#   % 2.3D-12*EXP(360/TEMP)*0.001 : CH3O2 + NO = CH3NO3 ;
#
# This may cause issues during the conversion to Fortran format by
# mech_converter.py. The script fixes these errors, if present, and
# returns the chemical mechanism properly formatted, e.g.:
#
#   % 3.8D-13*EXP(780/TEMP)*(1-1/(1+498*EXP(-1160/TEMP))) : CH3O2 + HO2 = CH3OOH ;
#   % 3.8D-13*EXP(780/TEMP)*(1/(1+498*EXP(-1160/TEMP))) : CH3O2 + HO2 = HCHO ;
#   % 2.3D-12*EXP(360/TEMP)*0.001 : CH3O2 + NO = CH3NO3 ;
#
# ARGUMENT:
#   1. path to the mechanism .fac file
# -------------------------------------------------------------------- #
from __future__ import print_function
from functools import reduce
import sys, re


# =========================== FUNCTIONS =========================== #


def fix_fac_full_contents(input_file):
    """
    Given a .fac file, return the contents of the file, but with
    incorrect newline characters removed and the affected lines
    concatenated correctly.

    WARNING: this will probably fail if a line is REALLY long,
    stretching over two full lines, but in this case it should
    then give an error as output.

    Args:
        input_file (str): name of the .fac file to be corrected

    Returns:
        fixed_file (list): corrected mechanism file, with each line as
                           a separate string
    """
    # Using splitlines() rather than readlines(), we take out the
    # errant carriage returns, and, for any line with such on it, we
    # return it to the list.
    with open(input_file, 'r') as file_open:
        contents = file_open.read().splitlines()

    # print contents
    orig_contents_len = len(contents)
    print(str(input_file) + ': file read in ' + str(orig_contents_len) + ' items.')
    contents_count = 0

    # This variable will hold the indices to be deleted once their
    # contents have been concatenated onto the previous element.
    to_delete = []

    # First, wait until we reach a line containing 'Reaction definitions'.
    # Second, ignore comment lines.
    # Third, correct the lines which don't start with a '%': they should
    # be concatenated onto the previous entry.
    in_reaction_definition_section = False
    for i in range(len(contents)):
        if not in_reaction_definition_section:
            # Check to see whether we are entering the 'Reaction definitions' section.
            if 'Reaction definitions.' in contents[i]:
                in_reaction_definition_section = True
        # Only do other checks if we've reached the 'Reaction definitions' section.
        else:
            if re.match(r'\*', contents[i]):
                pass
            else:
                if not re.match(r'%', contents[i]):
                    # print 'fail'
                    # print contents[i-1], 'XX', contents[i], 'XX', contents[i+1]
                    contents[i-1] += ' ' + contents[i]
                    # print contents[i-1]
                    contents_count += 1
                    to_delete.append(i)
    print(str(contents_count) + ' corrections made - now removing old.')

    # Remove old elements which have now been concatenated onto previous.
    for i in reversed(to_delete):
        del contents[i]
    assert orig_contents_len == contents_count + len(contents), str(input_file) \
        + ': file is probably too messed up with carriage returns for this script to fix.'

    # If there are any lines that have now been doubled-stacked, break them into pieces.
    # Find the end of the header section, because we don't want to parse that section
    # anymore. It often contains semicolons within the lines as well as at the end,
    # which breaks all our logic.
    end_of_header_index = [i for i, item in enumerate(contents) \
                           if re.search(r'Generic Rate Coefficients', item)]
    assert len(end_of_header_index) == 1
    end_of_header_index = end_of_header_index[0]

    # Split non-header lines by semicolons, but we keep the semicolons this way.
    interim_contents = [reduce(lambda acc, elem: acc[:-1] + [acc[-1] + elem] \
                               if elem == ";" else acc + [elem], re.split("(;)", element), []) \
                        for element in contents[end_of_header_index:]]

    # Remove empty sub-strings.
    interim_contents = [[item for item in sublist if item] for sublist in interim_contents]

    # Look for any lines containing more than 2 elements. These are lines where
    # more than one line is broken running together. At this point, the file is
    # too broken to easily fix manually - get the user to fix it and run again.
    if max([len(line) for line in interim_contents]) > 2:
        # Get index of line with error
        line_lengths = [len(line) for line in interim_contents]
        error_line = line_lengths.index(max(line_lengths)) + end_of_header_index + 1
        exit('The inputted file is broken near line ' + str(error_line) \
             + ' in a way that this script cannot handle.' \
             + ' Please manually fix this error and re-run this script.')

    # Reattach the header lines, and unwrap the list of lists in interim_contents.
    fixed_file = contents[:end_of_header_index] \
        + [item for sublist in interim_contents for item in sublist]
    return fixed_file

# ------------------------------------------------------------ #

def fix_fac_full_file(input_file):
    """
    Given a .fac file, overwrite the contents of the file with the
    same contents, but with incorrect newline characters removed and
    the affected lines concatenated correctly.

    All the work is done by the fix_fac_contents() function -- see its
    documentation for details. This function is just a simple wrapper
    to write the output of fix_fac_contents() back to the same file.

    Args:
        input_file (str): name of the .fac file to be fixed

    Returns:
        None
    """
    print('Running fix_fac_full_file on ' + str(input_file))
    contents = fix_fac_full_contents(input_file)
    contents = [item + '\n' for item in contents]
    with open(input_file, 'w') as file_open:
        file_open.writelines(contents)
    return


# =========================== MAIN =========================== #


def main():
    # Pass argument from command line - name of the .fac file to be fixed
    if len(sys.argv) > 1:
        fix_fac_full_contents(sys.argv[1])
    else:
        print('******************************************')
        print('Please pass a filename as script argument.')
        print('******************************************')
    return

# Call the main function if executed as script
if __name__ == '__main__':
    main()
