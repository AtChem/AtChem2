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

# --------------------------------------------------------------------
# This script converts a chemical mechanism file from KPP format
# (.kpp) to FACSIMILE format (.fac). The KPP file must have the same
# structure of the files generated by the MCM web extractor. A minimal
# example of this structure is: `mcm/mechanism_skel.kpp`.
#
# ARGUMENT:
#   1. path to the mechanism .kpp file
# -------------------------------------------------------------------- #
from __future__ import print_function
import sys
import re


# =========================== FUNCTIONS =========================== #


def mechanism_section(input_lines, start_section, end_section):
    """
    Parse the content of a file, provided in the form of a list
    (each line of the file an item of the list), and extract the
    section of the file delimited by two given markers.

    Args:
        input_lines (list): file to parse, each line is a separate string
        start_section (str): marker for the beginning of the section
        end_section (str): marker for the end of the section

    Returns:
        section_lines (list): file section, each line is a separate string

    """

    nlines = len(input_lines)

    start_i = 0
    for i in range(start_i, nlines):
        if start_section in input_lines[i]:
            start_i = i + 1
            break

    end_i = nlines
    if end_section:
        for i in range (start_i, nlines):
            if end_section in input_lines[i]:
                end_i = i + 1
                break

    section_lines = input_lines[start_i:end_i]
    return section_lines

# ------------------------------------------------------------ #

def convert_ro2(kpp_lines):
    """
    Converts the summation of organic peroxy radicals (RO2) to
    FACSIMILE format.

    Args:
        kpp_lines (list): lines with the RO2 sum in KPP

    Returns:
        fac_lines (list): lines with the RO2 sum in FACSIMILE
    """

    fac_lines = []
    for line in kpp_lines:
        new_line = re.sub(r'C\(ind_([A-Z0-9_]+)\s*\)', r'\1', line)
        new_line = re.sub(r'\s*&', r'', new_line.strip())
        fac_lines.append(new_line)
    return fac_lines

# ------------------------------------------------------------ #

def convert_rates(kpp_lines):
    """
    Converts the generic and complex rate coefficients to
    FACSIMILE format.

    Args:
        kpp_lines (list): lines with the rate coefficients in KPP

    Returns:
        fac_lines1 (list): lines with the generic rate coefficients in FACSIMILE
        fac_lines2 (list): lines with the complex rate coefficients in FACSIMILE

    """

    # list of generic rate coefficients -- this list may change with
    # future updates of the MCM
    simple_list = ['KRO2NO','KRO2HO2','KAPHO2','KAPNO','KRO2NO3','KNO3AL','KDEC',
                   'KROPRIM','KROSEC','KCH3O2','K298CH3O2','K14ISOM1']

    fac_lines1 = []
    fac_lines2 = []
    for line in kpp_lines:
        react_line = re.split(r'=', line)
        react_line[1] = react_line[1].replace('**', '@')
        new_line = react_line[0].strip() + ' = ' + react_line[1].strip() + ' ;\n'
        if react_line[0].strip() in simple_list:
            fac_lines1.append(new_line)
        else:
            fac_lines2.append(new_line)
    return fac_lines1, fac_lines2

# ------------------------------------------------------------ #

def convert_reactions(kpp_lines):
    """
    Converts the chemical reactions to FACSIMILE format.

    Args:
        kpp_lines (list): lines with the chemical reactions in KPP

    Returns:
        fac_lines (list): lines with the chemical reactions in FACSIMILE
    """

    fac_lines = []
    for line in kpp_lines:
        if re.match(r'{\d+\.}', line):
            react_line = re.split(r'[}:;]', line)
            rate_coeff = re.sub(r'J\((\d+)\)', r'J<\1>', react_line[2])
            rate_coeff = rate_coeff.replace('**', '@')
            new_line = '%' + rate_coeff + ':' + react_line[1] + ';\n'
            fac_lines.append(new_line)
    return fac_lines

# ------------------------------------------------------------ #

def kpp_to_facsimile(input_file):
    """Split a .kpp file into 4 sections: the summation of organic
    peroxy radicals (RO2), the generic and complex rate coefficients,
    the chemical reactions. Each section is separately converted to
    FACSIMILE format.

    WARNING: this function may fail if the .kpp file has a different
    structure from the .kpp files generated by the MCM web extractor.

    Args:
        input_file (str): name of the .kpp file to convert

    Returns:
        generic_rates (list): generic rate coefficients
        complex_reactions (list): complex rate coefficients
        peroxy_radicals (list): summation of organic peroxy radicals
        reaction_definitions (list): chemical reactions

    """

    # Read in the .kpp mechanism file
    with open(input_file, 'r') as file_open:
        contents = file_open.readlines()

    # Peroxy radicals section
    start_peroxy = 'RO2 = & '
    end_peroxy = ') \n'
    peroxy_lines = mechanism_section(contents, start_peroxy, end_peroxy)
    peroxy_radicals = convert_ro2(peroxy_lines)

    # Generic Rate Coefficients, Complex reactions sections
    start_rates = ') \n'
    end_rates = '#ENDINLINE'
    rates_lines = mechanism_section(contents, start_rates, end_rates)
    rates_lines = rates_lines[:-2]   # remove `CALL mcm_constants()` line
    generic_rates, complex_reactions = convert_rates(rates_lines)

    # Reaction definitions section
    start_reactions = '#EQUATIONS'
    end_reactions = ''   # file ends after list of reactions
    reactions_lines = mechanism_section(contents, start_reactions, end_reactions)
    reaction_definitions = convert_reactions(reactions_lines)

    # Sections of the mechanism file converted to KPP format
    return generic_rates, complex_reactions, peroxy_radicals, reaction_definitions

# ------------------------------------------------------------ #

def write_fac_file(input_file):
    """
    Convert a .kpp file to FACSIMILE format and save the output to a
    .fac file. The format conversion is done by the kpp_to_facsimile()
    function -- see its documentation for details.

    Args:
        input_file (str): name of the .kpp file to convert

    Returns:
        output_file (str): name of the converted .fac file
    """

    print('Running write_fac_file() on: ' + str(input_file))

    contents1, contents2, contents3, contents4 = kpp_to_facsimile(input_file)
    output_file = input_file.split('.')[0] + '.fac'

    with open(output_file, 'w') as file_open:
        file_open.write('\n* Generic Rate Coefficients ;\n')
        file_open.writelines(contents1)
        file_open.write('\n* Complex reactions ;\n')
        file_open.writelines(contents2)
        file_open.write('\n* Peroxy radicals ;\n')
        file_open.write('RO2 = ')
        file_open.writelines(contents3)
        file_open.write(';\n')
        file_open.write('\n* Reaction definitions ;\n')
        file_open.writelines(contents4)

    print(type(output_file))
    return output_file


# =========================== MAIN =========================== #


def main():
    # Pass argument from command line - name of the .kpp file to convert
    if len(sys.argv) > 1:
        write_fac_file(sys.argv[1])
    else:
        print('*****************************************************')
        print('* Please pass a filename (.kpp) as script argument. *')
        print('*****************************************************')

# Call the main function if executed as script
if __name__ == '__main__':
    main()
