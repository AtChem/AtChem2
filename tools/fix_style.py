import sys
import re

assert len(sys.argv) >= 2, "Please enter a filename as argument."

filename = sys.argv[1]
with open(filename, 'r') as input_file:
    lines = input_file.readlines()

with open(filename, 'w') as output_file:
    #print lines
    outputs = []
    for line in lines:
        # Replacements happen in reverse order on this line:
        # A comma followed by
        to_output = re.sub(",(?=[A-Z0-9('-])", ', ',
                           # Opening bracket preceded by an uppercase letter or a number should have a space in front.
                           re.sub('(?<=[A-Z0-9])\(', ' (',
                                  # Replace .LT. etc with lowercase and with no whitespace. This and the next 3 are all case-insensitive
                                  re.sub('\s*\.LT\.\s*', '.lt.',
                                         re.sub('\s*\.GT\.\s*', '.gt.',
                                                re.sub('\s*\.EQ\.\s*', '.eq.',
                                                       re.sub('\s*\.NE\.\s*', '.ne.',
                                                              # Any ending bracket should be followed by exactly one space if it's to be followed by a letter, digit, single-quote, or spaces
                                                              re.sub("\)(?=[\w\d\s'])", ') ',
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
                                                              flags=re.IGNORECASE),
                                                       flags=re.IGNORECASE),
                                                flags=re.IGNORECASE),
                                         flags=re.IGNORECASE),
                                  )
                           )
        # If it's a CALL line, then make the first opening bracket be preceded by exactly one space
        if re.match('\s*CALL \w+\s*\(', to_output):
            if re.search('(?<=[a-zA-Z0-9])\(', to_output):
                to_output = re.sub('(?<=[a-zA-Z0-9])\(', ' (', to_output, 1)
        # Append this line
        outputs.append(to_output)
    # Print all lines
    output_file.writelines(outputs)
    print 'Complete! Now run a find and replace by hand with regex "[^\\n^  !]  " to catch incorrect multiple-spaces.'
