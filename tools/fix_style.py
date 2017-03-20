import sys
import re

assert len(sys.argv) >= 2, "Please enter a filename as argument."

filename = sys.argv[1]
with open(filename, 'r') as input_file:
    lines = input_file.readlines()

with open(filename, 'w') as output_file:

    outputs = []
    for line in lines:
        # Replacements happen in reverse order on this line:
        # Any ending bracket followed by whitespace should be followed by exactly one space
        to_output = re.sub('\)[ \t]+', ') ',
               # Any ending bracket should be followed by exactly one space if it's to be followed by a letter, digit, or single-quote
               re.sub("\)(?=[\w\d'])", ') ',
                      # Any ending bracket followed by a double-quote, should have a single space.
                      re.sub('\)(?=")', ') ',
                             # Place all :: with exactly one space either side.
                             re.sub('\s*::\s*', ' :: ',
                                    # Replace e.g. ( Len =  by (LEN=
                                    re.sub('\(\s*LEN\s*=\s*', '(LEN=',
                                           line, flags=re.IGNORECASE
                                           )
                                    )
                             )
                      ),
               )

        # Replace .LT. etc with symbols and with no whitespace. These are all case-insensitive
        to_output = re.sub('\s*\.LT\.\s*', '<',
                           re.sub('\s*\.LE\.\s*', '<=',
                                  re.sub('\s*\.GT\.\s*', '>',
                                         re.sub('\s*\.GE\.\s*', '>=',
                                                re.sub('\s*\.EQ\.\s*', '==',
                                                       re.sub('\s*\.NE\.\s*', '/=',
                                                              to_output,
                                                              flags=re.IGNORECASE),
                                                       flags=re.IGNORECASE),
                                                flags=re.IGNORECASE),
                                         flags=re.IGNORECASE),
                                  flags=re.IGNORECASE),
                           flags=re.IGNORECASE)

        # A comma followed by any letter, digit, ( ' or - should have a space trailing.
        to_output = re.sub(",(?=[a-zA-Z0-9('-])", ', ',
                           # LOG10 and EXP are math functions so should be not followed by a space
                           re.sub('LOG10 \(', 'LOG10(',
                                  re.sub('EXP \(', 'EXP(',
                                         # Opening bracket preceded by an uppercase letter or a number should have a space in front.
                                         re.sub('(?<=[A-Z0-9])\(', ' (',
                                                to_output
                                                )
                                         )
                                  )
                           )
        # If it's a CALL line, then make the first opening bracket be preceded by exactly one space
        if re.match('\s*CALL \w+\s*\(', to_output):
            if re.search('(?<=[a-zA-Z0-9])\(', to_output):
                to_output = re.sub('(?<=[a-zA-Z0-9])\(', ' (', to_output, 1)
        # If it's a SUBROUTINE line, then make the first opening bracket be preceded by exactly one space
        if re.match('\s*SUBROUTINE \w+\s*\(', to_output):
            if re.search('(?<=[a-zA-Z0-9])\(', to_output):
                to_output = re.sub('(?<=[a-zA-Z0-9])\(', ' (', to_output, 1)
        # Append this line
        outputs.append(to_output)
    # Print all lines
    output_file.writelines(outputs)
    print 'Complete! Now run a find and replace by hand with regex "[^\\n^  !]  " to catch incorrect multiple-spaces.'
