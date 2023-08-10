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
# This script 
# -------------------------------------------------------------------- #
from __future__ import print_function
import sys
import re


# =========================== FUNCTIONS =========================== #


def kpp2fac(input_file):
    """
    """
    with open(input_file, 'r') as file_open:
        finlist = file_open.readlines()
        print(finlist)
        # mechlist = []
        # for reac in reaclist:
        #     if re.match(r'{\d+\.}', reac):
        #         xx = re.split(r'[}:;]', reac)
        #         kk = re.sub(r'J\((\d+)\)', r'J<\1>', xx[2])
        #         kk = kk.replace('**', '@')
        #         yy = "%" + kk + ":" + xx[1] + ";\n"
        #         mechlist.append(yy)
        #     return mechlist

# def writefac(mechlist, out_file):
#     """
#     """
#     with open(out_file, 'w') as file_open:
#         file_open.writelines(contents)
#         fout.write("* Generic Rate Coefficients ;\n")
#         fout.write("* Complex reactions ;\n")
#         fout.write("* Peroxy rdicals. ;\n")
#         fout.write("RO2 = ;\n")
#         fout.write("* Reaction definitions. ;\n")
#         for r in mechlist:
#             fout.write(r)
#     return


# =========================== MAIN =========================== #


def main():
    # Pass argument from command line - name of the .kpp file to be converted
    if len(sys.argv) > 1:
        kpp2fac(sys.argv[1])
    else:
        print('******************************************')
        print('Please pass a filename as script argument.')
        print('******************************************')

# Call the main function if executed as script
if __name__ == '__main__':
    main()
