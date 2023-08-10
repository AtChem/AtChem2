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
#
# ARGUMENT:
#   1.
# -------------------------------------------------------------------- #
from __future__ import print_function
import sys
import re


# =========================== FUNCTIONS =========================== #


def extract_section(input_lines, start_marker, end_marker):
    """
    Extracts lines between start_marker and end_marker from input_lines.
    """
    nlines = len(input_lines)

    start_i = 0
    for i in range(start_i, nlines):
        if start_marker in input_lines[i]:
            start_i = i + 1
            break

    end_i = nlines
    if end_marker:
        for i in range (start_i, nlines):
            if end_marker in input_lines[i]:
                end_i = i + 1
                break

    section_lines = input_lines[start_i:end_i]
    return section_lines

# ------------------------------------------------------------ #

def convert_rates(section_lines):
    """
    Converts the format of section_lines and returns the converted lines.
    """
    simplelist = ['KRO2NO','KRO2HO2','KAPHO2','KAPNO','KRO2NO3','KNO3AL','KDEC',
                  'KROPRIM','KROSEC','KCH3O2','K298CH3O2','K14ISOM1']
    mechlist1 = []
    mechlist2 =[]
    for reac in section_lines: #kk = kk.replace('**', '@')
        xx = re.split(r'=', reac)
        if xx[0].strip() in simplelist:
            mechlist1.append(reac)
        else:
            mechlist2.append(reac)
    return mechlist1, mechlist2

# ------------------------------------------------------------ #

def convert_ro2(section_lines):
    """
    Converts the format of section_lines and returns the converted lines.
    """
    mechlist = []
    for line in section_lines:
        cleaned_line = re.sub(r'C\(ind_([A-Z0-9_]+)\s*\)', r'\1', line)
        cleaned_line = re.sub(r'\s*&', r'', cleaned_line.strip())
        mechlist.append(cleaned_line)
    return mechlist

# ------------------------------------------------------------ #

def convert_reactions(section_lines):
    """
    Converts the format of section_lines and returns the converted lines.
    """
    mechlist = []
    for reac in section_lines:
        if re.match(r'{\d+\.}', reac):
            xx = re.split(r'[}:;]', reac)
            kk = re.sub(r'J\((\d+)\)', r'J<\1>', xx[2])
            kk = kk.replace('**', '@')
            yy = '%' + kk + ':' + xx[1] + ';\n'
            mechlist.append(yy)
    return mechlist

# ------------------------------------------------------------ #

def kpp_to_facsimile(input_file):
    """
    workhorse function
    """

    with open(input_file, 'r') as file_open:
        contents = file_open.readlines()

    start_ro2 = 'RO2 = & '
    end_ro2 = ') \n'
    ro2_lines = extract_section(contents, start_ro2, end_ro2)
    converted_ro2 = convert_ro2(ro2_lines)

    start_rates = end_ro2
    end_rates = '#ENDINLINE'
    rates_lines = extract_section(contents, start_rates, end_rates)[:-2]
    converted_rate1, converted_rate2 = convert_rates(rates_lines)

    start_reactions = '#EQUATIONS'
    end_reactions = ''
    reactions_lines = extract_section(contents, start_reactions, end_reactions)
    converted_reactions = convert_reactions(reactions_lines)

    #
    return converted_rate1, converted_rate2, converted_ro2, converted_reactions

# ------------------------------------------------------------ #

def write_fac_file(input_file):
    """
    wrapper
    """
    print('Running write_fac_file on ' + str(input_file))
    contents1, contents2, contents3, contents4 = kpp_to_facsimile(input_file)
    output_file = input_file.split('.')[0] + '.fac1'
    with open(output_file, 'w') as file_open:
        file_open.write('\n* Generic Rate Coefficients ;\n')
        file_open.writelines(contents1)
        file_open.write('\n* Complex reactions ;\n')
        file_open.writelines(contents2)
        file_open.write('\n* Peroxy radicals. ;\n')
        file_open.write('RO2 = ')
        file_open.writelines(contents3)
        file_open.write(';\n')
        file_open.write('\n* Reaction definitions. ;\n')
        file_open.writelines(contents4)


# =========================== MAIN =========================== #


def main():
    # Pass argument from command line - name of the .kpp file to be converted
    if len(sys.argv) > 1:
        #aa = kpp_to_facsimile(sys.argv[1])
        write_fac_file(sys.argv[1])
    else:
        print('******************************************')
        print('Please pass a filename as script argument.')
        print('******************************************')

# Call the main function if executed as script
if __name__ == '__main__':
    main()
