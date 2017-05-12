# Written by Sam Cox, University of Leicester, 2017
#
# This script rewrites the source file provided as an argument, replacing it with a copy that better fits the
# expected style. If two arguments are given, the output wil be written to the second, leaving the first untouched.
#
# WARNING: Please note that this script is not infallible, and can break your code! Please use with caution,
# and make sure you have a copy of your source file to revert to in the event of it breaking.
#


def replace_any_case_with_lower_first(string, to_output):
    # Replaces the first word(s) with its lowercase if it matches string
    if re.match('^\s*'+string, to_output, flags=re.IGNORECASE):
        to_output = re.sub(string, string.lower(), to_output, 1, flags=re.IGNORECASE)
    return to_output

def replace_any_case_with_lower(string, to_output):
    # Replaces the first word(s) with its lowercase if it matches string
    if re.search(string, to_output.upper(), flags=re.IGNORECASE):
        to_output = re.sub(string, string.lower(), to_output, 1, flags=re.IGNORECASE)
    return to_output

def brackets_for_calls(string, to_output, is_first_line_of_multiline, is_inside_procedure, currently_on_multiline):
    # Sets first bracket to be preceded by no whitespace, followed by one space
    if re.match('^\s*'+string, to_output, flags=re.IGNORECASE):
        is_inside_procedure = True
        to_output = replace_any_case_with_lower_first(string, to_output)
        if is_first_line_of_multiline or not currently_on_multiline:
            if re.search('(?<=[a-zA-Z0-9\s])\(', to_output):
                to_output = re.sub('\s*\(\s*(?=[a-zA-Z0-9\s\'\"\)])', '( ', to_output, 1)
    return to_output, is_inside_procedure, currently_on_multiline

def strip_newline(string):
    string = re.sub('\n', '', string)
    return string

def add_newline(string):
    string = string + '\n'
    return string

def add(string1, string2):
    string = add_newline(strip_newline(string1)+string2)
    return string

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

# loop for fixing style
with open(out_filename, 'w') as output_file:
    # set up variable for the first time
    currently_on_multiline = False
    previous_line_ends_ampersand = False
    is_inside_procedure = False
    outputs = []
    for line in lines:
        # extract comment from end of line, to append later
        line = strip_newline(line)
        split_line = line.split('!')
        to_output = split_line[0]
        if len(split_line) > 1:
            # cycle over each combo of strings
            for i in range(len(split_line)):
                to_output = '!'.join(split_line[0:i+1])
                if even_quotes(to_output):
                    comment = strip_newline('!'+'!'.join(split_line[i+1:]))
                    break
        else:
            comment = ''

        if to_output.isspace() or to_output == '':
            empty_line = True

        # book-keeping for matching bracketed calls over multiple lines
        this_line_ends_ampersand = False
        is_first_line_of_multiline = False

        # Handle the case where this line is split onto the next line(s)
        if re.search('\&\s*$', to_output):
            this_line_ends_ampersand = True
            if not currently_on_multiline:
                is_first_line_of_multiline = True
            currently_on_multiline = True

        # Replace .LT. etc with symbols.
        to_output = re.sub('\s*\.LT\.\s*', ' < ',  to_output, flags=re.IGNORECASE)
        to_output = re.sub('\s*\.LE\.\s*', ' <= ', to_output, flags=re.IGNORECASE)
        to_output = re.sub('\s*\.GT\.\s*', ' > ',  to_output, flags=re.IGNORECASE)
        to_output = re.sub('\s*\.GE\.\s*', ' >= ', to_output, flags=re.IGNORECASE)
        to_output = re.sub('\s*\.EQ\.\s*', ' == ', to_output, flags=re.IGNORECASE)
        to_output = re.sub('\s*\.NE\.\s*', ' /= ', to_output, flags=re.IGNORECASE)

        # put one space after each comma, except where followed by * or :
        to_output = re.sub(',\s*', ', ', to_output)
        to_output = re.sub(', \*', ',*', to_output)
        to_output = re.sub(', \:', ',:', to_output)

        # Replace e.g. ( Len =  by (LEN=, etc.
        to_output = re.sub('\(LEN\s*=',  '(len=',   to_output, flags=re.IGNORECASE)
        to_output = re.sub('\(KIND\s*=', '(kind=',  to_output, flags=re.IGNORECASE)
        to_output = re.sub('STATUS\s*=', 'status=', to_output, flags=re.IGNORECASE)
        to_output = re.sub('IOSTAT\s*=', 'iostat=', to_output, flags=re.IGNORECASE)
        to_output = re.sub('FILE\s*=',   'file=',   to_output, flags=re.IGNORECASE)
        to_output = re.sub('EXIST\s*=',  'exist=',  to_output, flags=re.IGNORECASE)

        # Any ending bracket followed by a double-quote, should have a single space.
        to_output = re.sub('\)(?=")', ') ', to_output)
        # Any ending bracket should be followed by exactly one space if it's to be followed by a letter, digit, or single-quote
        to_output = re.sub("\)(?=[\w\d'])", ') ', to_output)
        # Any ending bracket already followed by whitespace should be followed by exactly one space
        to_output = re.sub('\)[ \t]+', ') ', to_output)

        # These are math functions, so should be lower-case
        to_output = re.sub('ABS\(',   'abs(',   to_output, flags=re.IGNORECASE)
        to_output = re.sub('LOG10\(', 'log10(', to_output, flags=re.IGNORECASE)
        to_output = re.sub('EXP\(',   'exp(',   to_output, flags=re.IGNORECASE)

        # There are intrinsic functions, and should be lowercase
        to_output = re.sub('TRIM\(',   'trim(',      to_output, flags=re.IGNORECASE)
        to_output = re.sub('ADJUSTL\(','adjustl(',   to_output, flags=re.IGNORECASE)
        to_output = re.sub('ADJUSTR\(','adjustr(',   to_output, flags=re.IGNORECASE)

        # A comma followed by any letter, digit, ( ' or - should have a space trailing.
        to_output = re.sub(",(?=[a-zA-Z0-9('-])", ', ', to_output)

        # These are modifiers so should be lowercase
        to_output = re.sub(',\s*ONLY\s*:\s*',   ', only : ',    to_output, flags=re.IGNORECASE)
        to_output = re.sub(',\s*OPTIONAL\s*',   ', optional',   to_output, flags=re.IGNORECASE)
        to_output = re.sub(',\s*CONTIGUOUS\s*', ', contiguous', to_output, flags=re.IGNORECASE)
        to_output = re.sub(',\s*PARAMETER\s*',  ', parameter',  to_output, flags=re.IGNORECASE)
        to_output = re.sub(',\s*PRIVATE\s*',    ', private',    to_output, flags=re.IGNORECASE)
        to_output = re.sub(',\s*PUBLIC\s*',     ', public',     to_output, flags=re.IGNORECASE)

        # Place all :: with exactly one space either side.
        to_output = re.sub('\s*::\s*', ' :: ', to_output)

        # If it's a CALL etc line, then make the first opening bracket be preceded by no whitespace,
        # last bracket to be preceded by one space (while handling the case where this is split over multiple lines)
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls('CALL',          to_output, is_first_line_of_multiline, is_inside_procedure, currently_on_multiline)
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls('SUBROUTINE',    to_output, is_first_line_of_multiline, is_inside_procedure, currently_on_multiline)
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls('FUNCTION',      to_output, is_first_line_of_multiline, is_inside_procedure, currently_on_multiline)
        to_output, is_inside_procedure, currently_on_multiline = brackets_for_calls('PURE FUNCTION', to_output, is_first_line_of_multiline, is_inside_procedure, currently_on_multiline)

        # Convert all these keywords, which start statements, to lowercase
        to_output = replace_any_case_with_lower_first('USE', to_output)
        to_output = replace_any_case_with_lower_first('IMPLICIT NONE', to_output)
        to_output = replace_any_case_with_lower_first('MODULE', to_output)
        to_output = replace_any_case_with_lower_first('CONTAINS', to_output)
        to_output = replace_any_case_with_lower_first('END SUBROUTINE', to_output)
        to_output = replace_any_case_with_lower_first('END FUNCTION', to_output)
        to_output = replace_any_case_with_lower_first('END MODULE', to_output)
        to_output = replace_any_case_with_lower_first('INTEGER', to_output)
        to_output = replace_any_case_with_lower_first('REAL', to_output)
        to_output = replace_any_case_with_lower_first('CHARACTER', to_output)
        to_output = replace_any_case_with_lower_first('LOGICAL', to_output)
        to_output = replace_any_case_with_lower_first('RETURN', to_output)
        to_output = replace_any_case_with_lower_first('DO', to_output)
        to_output = replace_any_case_with_lower_first('DO WHILE', to_output)
        to_output = replace_any_case_with_lower_first('END DO', to_output)
        to_output = replace_any_case_with_lower_first('ELSE', to_output)
        to_output = replace_any_case_with_lower_first('WRITE', to_output)
        to_output = replace_any_case_with_lower_first('READ', to_output)
        to_output = replace_any_case_with_lower_first('INQUIRE', to_output)
        to_output = replace_any_case_with_lower_first('PRINT', to_output)
        to_output = replace_any_case_with_lower_first('STOP', to_output)
        to_output = replace_any_case_with_lower_first('EXIT', to_output)
        to_output = replace_any_case_with_lower_first('OPEN', to_output)
        to_output = replace_any_case_with_lower_first('CLOSE', to_output)
        to_output = replace_any_case_with_lower_first('INCLUDE', to_output)
        to_output = replace_any_case_with_lower_first('IF', to_output)
        to_output = replace_any_case_with_lower_first('END IF', to_output)
        to_output = replace_any_case_with_lower_first('SELECT CASE', to_output)
        to_output = replace_any_case_with_lower_first('CASE', to_output)
        to_output = replace_any_case_with_lower_first('END SELECT', to_output)
        to_output = replace_any_case_with_lower_first('ALLOCATE', to_output)
        to_output = replace_any_case_with_lower_first('DEALLOCATE', to_output)
        to_output = replace_any_case_with_lower_first('DATA', to_output)
        to_output = replace_any_case_with_lower_first('PRIVATE', to_output)
        to_output = replace_any_case_with_lower_first('PUBLIC', to_output)

        # Split 'enddo' and 'endif' into two words, lowercase
        if re.match('\s*enddo', to_output, flags=re.IGNORECASE):
            to_output = re.sub('enddo', 'end do', to_output, 1, flags=re.IGNORECASE)
        if re.match('\s*endif', to_output, flags=re.IGNORECASE):
            to_output = re.sub('endif', 'end if', to_output, 1, flags=re.IGNORECASE)

        # These are all modifiers, so should be lowercase
        to_output = replace_any_case_with_lower('intent(in)', to_output)
        to_output = replace_any_case_with_lower('intent(inout)', to_output)
        to_output = replace_any_case_with_lower('intent(out)', to_output)
        to_output = replace_any_case_with_lower('allocatable', to_output)
        to_output = replace_any_case_with_lower('intrinsic', to_output)

        # Replace these value and relational operators with lowercase
        to_output = re.sub('\.true\.',  '.true.',  to_output, flags=re.IGNORECASE)
        to_output = re.sub('\.false\.', '.false.', to_output, flags=re.IGNORECASE)
        to_output = re.sub('\.eqv\.',   '.eqv.',   to_output, flags=re.IGNORECASE)
        to_output = re.sub('\.not\.',   '.not.',   to_output, flags=re.IGNORECASE)
        to_output = re.sub('\.or\.',    '.or.',    to_output, flags=re.IGNORECASE)
        to_output = re.sub('\.and\.',   '.and.',   to_output, flags=re.IGNORECASE)

        # If it's a INTEGER, REAL or CHARACTER line, then make the first opening bracket be preceded by no whitespace,
        # first closing bracket to be preceded by one space unless there's a comma.
        if (re.match('\s*integer\s*\(', to_output)
          or re.match('\s*real\s*\(', to_output)
          or re.match('\s*character\s*\(', to_output)):
            to_output = re.sub('\s*\(', '(', to_output, 1)
            to_output = re.sub('\)[^,]\s*', ') ', to_output, 1)

        # Match if-thens, give one space and lowercase
        if re.search('\s*IF.+THEN', to_output, flags=re.IGNORECASE):
            to_output = re.sub('IF\s*', 'if ', to_output, flags=re.IGNORECASE)
            to_output = re.sub('\s*THEN', ' then', to_output, flags=re.IGNORECASE)

        # add space before last bracket of preocedure call or definition, handling mutliple lines
        if is_inside_procedure and not this_line_ends_ampersand:
            if re.search('(?<=[a-zA-Z0-9])\)', to_output):
                to_output = re.sub(r"\s*\)(?=[^\)]*$)", r" )", to_output)
            is_inside_procedure = False

        # convert any ( ) to ()
        to_output = re.sub('\s*\(\s*\)\s*', '()', to_output)

        # separate ')result' to ') result'
        if re.match('^\s*FUNCTION', to_output, flags=re.IGNORECASE) or re.match('^\s*PURE FUNCTION', to_output, flags=re.IGNORECASE):
            to_output = re.sub('\)result \(', ') result (', to_output, flags=re.IGNORECASE)

        # end multiline environment if this doesn't end with an ampersand
        if not this_line_ends_ampersand:
            currently_on_multiline = False

        outputs.append(to_output + add_newline(comment))

    output_file.writelines(outputs)
    print 'Complete! Now run a find and replace by hand with regex "[^\\n^  !]  " to catch incorrect multiple-spaces.'
