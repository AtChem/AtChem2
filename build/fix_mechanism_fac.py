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

# This script contains functions to fix the contents of a chemical
# mechanism file in FACSIMILE format (.fac ) by removing the incorrect
# newline characters.
#
# ARGUMENT:
# - path to the .fac file
# ---------------------------------------------- #
from __future__ import print_function
from functools import reduce
import sys
import re


## ------------------------------------------------------------------ ##


def fix_fac_full_contents(filename):
    # Given a filename, return the contents of the file, but with
    # incorrect newline characters removed, and the affected lines
    # concatenated correctly. This will probably fail if a line is
    # REALLY long, stretching over two full lines, but should probably
    # then give an error as output.
    #
    # Using splitlines rather than readlines(), we take out the errant
    # carriage returns, and for any line with such on it, we return to
    # members of the list.
    with open(filename, 'r') as file_open:
        contents = file_open.read().splitlines()

    # print contents
    orig_contents_len = len(contents)
    print(str(filename) + ': file read in ' + str(orig_contents_len) + ' items')
    contents_count = 0

    # This variable will hold the indices to be deleted once their contents have
    # been concatenated onto the previous element.
    to_delete = []

    # First, wait until we reach a line containing 'Reaction definitions'.
    # Then ignore comment lines.
    # Then correct the lines which don't start with a '%': they should be concatenated onto the previous entry.
    in_reaction_definition_section = False
    for i in range(len(contents)):
        if not in_reaction_definition_section:
            # Check to see whether we are entering the 'Reaction definitions' section
            if 'Reaction definitions.' in contents[i]:
                in_reaction_definition_section = True
        # Only do other checks if we've reached 'Reaction definitions' sections
        else:
            if re.match(r'\*', contents[i]):
                pass
            else:
                if not re.match(r'%', contents[i]):
                    # print 'fail'
                    # print contents[i - 1], 'XX', contents[i], 'XX', contents[i + 1]
                    contents[i - 1] += ' ' + contents[i]
                    # print contents[i - 1]
                    contents_count += 1
                    to_delete.append(i)
    print(str(contents_count) + ' corrections made - now removing old')

    # Remove old elements which have now been concatenated onto previous
    for i in reversed(to_delete):
        del contents[i]
    assert orig_contents_len == contents_count + len(contents), \
        str(filename) + ': file is probably too messed up with carriage returns for this simple script to fix.'

    # If there are any lines that have now been doubled-stacked, then break them into pieces.
    # Find the end of the header section, because we don't want to parse that section anymore - it
    # often contains semicolons within the lines as well as at the end, which breaks all our logic
    end_of_header_index = [i for i, item in enumerate(contents) if re.search(r'Generic Rate Coefficients', item)]
    assert len(end_of_header_index) == 1
    end_of_header_index = end_of_header_index[0]

    # Split non-header lines by semicolons, but we keep the semicolons this way.
    interim_contents = [reduce(lambda acc, elem: acc[:-1] + [acc[-1] + elem] if elem == ";" else acc + [elem], re.split("(;)", element), []) for element in contents[end_of_header_index:]]

    # Remove empty sub-strings
    interim_contents = [[item for item in sublist if item] for sublist in interim_contents]

    # Look for any lines containing more than 2 elements. These are lines where more than
    # one line is broken running together. At this point, the file is too broken to
    # easily fix manually - get the user to fix it and run again.
    if max([len(line) for line in interim_contents]) > 2:
        # Get index of line with error
        error_line = ([len(line) for line in interim_contents]).index(max([len(line) for line in interim_contents])) + end_of_header_index + 1
        exit('The inputted file is broken near to line ' + str(error_line) + ' in a way that this script cannot handle.' +
             ' Please manually fix this error and re-run this script.')

    # Reattach the header lines, and unwrap the list of lists in interim_contents
    final_list = contents[:end_of_header_index] + [item for sublist in interim_contents for item in sublist]
    return final_list


def fix_fac_full_file(filename):
    # Given a filename, overwrite the contents of the file with the same contents,
    # but with incorrect newline characters removed, and the affected lines
    # concatenated correctly. This will probably fail if a line is REALLY long,
    # stretching over two full lines, but should probably then give an error as output.
    # All the heavy lifting is done by fix_fac_contents - here we just provide a
    # simple wrapper to write back to the same file.
    print('Running fix_fac_file on ' + str(filename))
    contents = fix_fac_full_contents(filename)
    contents = [item + '\n' for item in contents]
    with open(filename, 'w') as file_open:
        file_open.writelines(contents)
    return


## ------------------------------------------------------------------ ##


def main():
    # Pass argument from command line as path to file.
    if len(sys.argv) > 1:
        fix_fac_full_contents(sys.argv[1])
    else:
        print('******************************')
        print("Please pass a filename as argument. This script will then fix this file to remove incorrect newlines.")
        print('******************************')
    return

if __name__ == '__main__':
    main()
