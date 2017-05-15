# Written by Sam Cox, University of Leicester, 2017
#
# This script indents the source file provided as an argument, replacing it with a copy that better fits the
# expected style. If two arguments are given, the output wil be written to the second, leaving the first untouched.
#
# WARNING: Please note that this script is not infallible, and could break your code! Please use with caution,
# and make sure you have a copy of your source file to revert to in the event of it breaking.
#
# The workflow of this script is quite simple. For each line:
#  * strip off the trailing newline
#  * strip out the comment from the end of the line, relying on the fact that quotes
#      are not able to span lines, to ensure we really grab a '!' marking the comment start, and not in a string literal.
#  * if the non-comment content is all whitespace, then skip to reappending the comment, otherwise:
#  * if the previous line was e.g. the start of a procedure, then increase the indent on this line.
#  * store whether this line ends in an ampersand
#  * if the previous line ended in an ampersand, then we don't know where to align to, so leave untouched.
#    * otherwise, 'end', 'else' or 'contains' means this line should be unindented.
#    * if this line is the start of e.g. a procedure, store that the next line (that isn't following an ampersand) should be indented.
#  * perform appropriate indentation, rejoin comment and newline, and output.
#  * set up variables for next loop, passing on indent setting and whether this was an ampersand-ending line.

# strip newline characters from string
def strip_newline(string):
    string = re.sub('\n', '', string)
    return string

# append newline character to string
def add_newline(string):
    string = string + '\n'
    return string

# count whether there are an even number of both single- and double-quotes in the given string.
def even_quotes(string):
    if string.count('"') % 2 == 0:
        double = True
    else:
        double = False
    if string.count("'") % 2 == 0:
        single = True
    else:
        single = False
    return (double and single)

import sys
import re

# handle input arguments. If only one is provided, use this for input and output. Error if none provided.
assert len(sys.argv) >= 2, "Please enter a filename as argument."
filename = sys.argv[1]
if len(sys.argv) == 3:
    out_filename = sys.argv[2]
else:
    out_filename = filename

# read in file contents
with open(filename, 'r') as input_file:
    lines = input_file.readlines()

# loop for indenting
with open(out_filename, 'w') as output_file:
    # set up variables for first time
    previous_line_ends_ampersand = False
    this_line_ends_ampersand = False
    next_line_indent_more = False
    empty_line = False
    start_select = False
    indent = 0
    outputs = []
    for line in lines:
        empty_line = False
        # extract comment from end of line, to append later
        line = strip_newline(line)
        split_line = line.split('!')
        to_output = split_line[0]
        if len(split_line) > 1:
            # cycle over each combo of strings until we split command from comment
            for i in range(len(split_line)):
                to_output = '!'.join(split_line[0:i+1])
                if even_quotes(to_output):
                    comment = strip_newline('!'+'!'.join(split_line[i+1:]))
                    break
        else:
            comment = ''

        if to_output.isspace() or to_output == '':
            empty_line = True

        if next_line_indent_more:
            indent = indent+1
            next_line_indent_more = False

        if not empty_line:
            this_line_ends_ampersand = False
            # check whether this line ends in ampersand
            if re.search('&\s*$', to_output):
                this_line_ends_ampersand = True

            if not previous_line_ends_ampersand:
                # check if this line starts with 'end' : next line should be unindented
                if re.match('^\s*end\s*', to_output, flags=re.IGNORECASE) \
                  or re.match('^\s*else\s*', to_output, flags=re.IGNORECASE) \
                  or re.match('^\s*contains\s*', to_output, flags=re.IGNORECASE):
                    indent = indent-1

                # Handle the fact that each case of a select structure doesn't end in an 'end', so the 'end selec' needs to go back twice.
                if re.match('^\s*end select\s*', to_output, flags=re.IGNORECASE):
                    indent = indent - 1

                # match if, do, subroutine, function, module, else, contains, program, interface, select to set the next line indent higher
                if re.search('\s*IF.+THEN', to_output, flags=re.IGNORECASE) or re.match('^\s*else\s*', to_output, flags=re.IGNORECASE) or re.match('^\s*do\s*', to_output, flags=re.IGNORECASE) \
                    or re.match('^\s*subroutine\s*', to_output, flags=re.IGNORECASE) or re.match('^\s*function\s*', to_output, flags=re.IGNORECASE) \
                    or re.match('^\s*module\s*', to_output, flags=re.IGNORECASE) or re.match('^\s*contains\s*', to_output, flags=re.IGNORECASE) \
                    or re.match('^\s*program\s*', to_output, flags=re.IGNORECASE) or re.match('^\s*interface\s*', to_output, flags=re.IGNORECASE) \
                    or re.match('^\s*pure function\s*', to_output, flags=re.IGNORECASE) or re.match('^\s*select\s*', to_output, flags=re.IGNORECASE) \
                    or re.match('^\s*case\s*', to_output, flags=re.IGNORECASE):
                    next_line_indent_more = True

                # set start_select when we enter a select structure
                if re.match('^\s*select\s*', to_output, flags=re.IGNORECASE):
                    start_select = True

                # if at a 'case' statement, check whether it's the first one, via start_select
                # - if so, don't change the indent, as we just want it to be indented next time
                # otherwise, unindent by one
                if re.match('^\s*case\s*', to_output, flags=re.IGNORECASE):
                    if start_select:
                        start_select = False
                    else:
                        indent = indent-1

            else:
                next_line_indent_more = False

        if not previous_line_ends_ampersand:
            if re.search('\S', to_output):
                to_output = re.sub('^\s*(?=\S)', ' '*2*indent, to_output)
            elif re.search('\S', to_output + comment):
                to_output = ''
                comment = (' '*2*indent)+comment

        if this_line_ends_ampersand:
            previous_line_ends_ampersand = True
        else:
            previous_line_ends_ampersand = False

        outputs.append(to_output + add_newline(comment))

    output_file.writelines(outputs)
    print 'Complete! Now run a find and replace by hand with regex "&\s*\\n" to catch alignment of the lines following ampersands.'
