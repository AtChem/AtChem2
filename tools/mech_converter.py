# -----------------------------------------------------------------------------
#
# Copyright (c) 2009 - 2012 Chris Martin, Kasia Boronska, Jenny Young,
# Peter Jimack, Mike Pilling
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

# This program fixes the input file of errant newlines, then output the reactants, products, species list, and rates.
# This only reads a file containing the 'reaction definitions' part.
import re
import os
import sys
import fix_mechanism_fac


def convert(input_file, output_dir, mc_dir, mcm_dir):
    input_directory = os.path.dirname(os.path.abspath(input_file))
    input_filename = os.path.basename(input_file)
    assert os.path.isfile(os.path.join(input_directory, input_filename)), 'The input file ' + str(
        os.path.join(input_directory, input_filename)) + ' does not exist.'
    print input_directory
    # Fix the input contents of any errant newlines
    fix_mechanism_fac.fix_fac_full_file(os.path.join(input_directory, input_filename))

    # Read in the input file
    print 'Reading input file'
    with open(os.path.join(input_directory, input_filename), 'r') as input_file:
        s = input_file.readlines()

    # split the lines into the following sections:
    # - Ignore everything up to Generic Rate Coefficients
    # - Generic Rate Coefficients
    # - Complex reactions
    # - Peroxy radicals
    # - Reaction definitions
    section_headers_indices = [0, 1, 2, 3]
    section_headers = ['Generic Rate Coefficients', 'Complex reactions', 'Peroxy radicals', 'Reaction definitions']
    generic_rate_coefficients = []
    complex_reactions = []
    peroxy_radicals = []
    reaction_definitions = []

    section = 0
    for line in s:
        for header_index in section_headers_indices:
            if section_headers[header_index] in line:
                section += 1
        if section == 1:
            generic_rate_coefficients.append(line)
        elif section == 2:
            complex_reactions.append(line)
        elif section == 3:
            peroxy_radicals.append(line)
        elif section == 4:
            reaction_definitions.append(line)
        else:
            assert section == 0, "Error, section is not in [0,4]"

    # Initialise a few variables
    speciesList = []
    rateConstants = []
    reactionNumber = 0

    with open(os.path.join(mc_dir, 'mechanism.reac'), 'w') as reac_file, open(
            os.path.join(mc_dir, 'mechanism.prod'), 'w') as prod_file:
        mech_reac_list = []
        mech_prod_list = []
        # Loop over all lines in the reaction_definitions section of the input file
        for line in reaction_definitions:

            # Check for comments (beginning with a !), or blank lines
            if (re.match('!', line) is not None) | (line.isspace()):
                rateConstants.append(line)
            # Check for lines starting with either ; or *, and write these as comments
            elif (re.match(';', line) is not None) | (re.match('[*]', line) is not None):
                rateConstants.append('!' + line)
            # Otherwise assume all remaining lines are in the correct format, and so process them
            else:
                # reactionNumber keeps track of the line we are processing
                reactionNumber += 1

                # strip whitespace, ; and %
                line = line.strip().strip('%;').strip()

                # split by the semi-colon : a[0] is reaction rate, a[1] is reaction equation
                a = re.split(':', line)

                # Add reaction rate to rateConstants
                rateConstants.append(a[0])

                # Process the reaction: split by = into reactants and products
                reaction_parts = re.split('=', a[1])

                reactantsList = reaction_parts[0]
                productsList = reaction_parts[1]

                # Process each of reactants and products by splitting by +. Strip each at this stage.
                reactants = [item.strip() for item in re.split('[+]', reactantsList)]
                products = [item.strip() for item in re.split('[+]', productsList)]

                # Ignore empty reactantsList
                if not reactantsList == '':
                    # Compare each reactant against known species.
                    reactantNums = []
                    for x in reactants:
                        # If the reactant is a known species then add its number to reactantNums
                        if x in speciesList:
                            reactantNums.append(speciesList.index(x)+1)
                        else:
                            # Reactant x is not a known species.
                            # Add reactant to speciesList, and add this number to
                            # reactantNums to record this reaction.
                            speciesList.append(x)
                            reactantNums.append(len(speciesList))
                            # print 'adding', x, 'to speciesList'

                    # Write the reactants to mech_reac_list
                    mech_reac_list.extend([str(reactionNumber) + ' ' + str(z) + '\n' for z in reactantNums])

                if not productsList == '':
                    # Compare each product against known species.
                    productNums = []
                    for x in products:
                        # If the reactant is a known species then add its number to reactantNums
                        if x in speciesList:
                            productNums.append(speciesList.index(x)+1)
                        else:
                            # Product x is not a known species.
                            # Add product to speciesList, add this number to
                            # productNums to record this reaction.
                            speciesList.append(x)
                            productNums.append(len(speciesList))
                            # print 'adding', x, 'to speciesList'

                    # Write the products to mechanism.prod
                    for z in productNums:
                        mech_prod_list.append(str(reactionNumber) + ' ' + str(z) + '\n')


        # Output number of species and number of reactions
        prod_file.write(str(len(speciesList)) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions\n')
        # Write all other lines
        for line in mech_prod_list:
            prod_file.write(line)

        # Output number of species and number of reactions
        reac_file.write(str(len(speciesList)) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions\n')

        # Write all other lines
        for line in mech_reac_list:
            reac_file.write(line)


    # Write speciesList to mechanism.species, indexed by (1 to len(speciesList))
    with open(os.path.join(mc_dir, 'mechanism.species'), 'w') as species_file:
        for i, x in zip(range(1, len(speciesList) + 1), speciesList):
            species_file.write(str(i) + ' ' + str(x) + '\n')

    # Write out rate coefficients
    i = 1
    mech_rates_list = []
    for rate_counter, x in zip(range(len(s)), rateConstants):
        if (re.match('!', x) is not None) | (x.isspace()):
            mech_rates_list.append(str(x))
        else:
            # This matches anything like @-dd.d and replaces with **(-dd.d). This uses (?<=@) as a lookbehind assertion,
            # then matches - and any combination of digits and decimal points. This replaces the negative number by its
            # bracketed version.
            string = re.sub('(?<=@)-[0-9.]*', '(\g<0>)', x)
            # Now convert all @ to ** etc.
            string = string.replace('@', '**')
            string = string.replace('<', '(')
            string = string.replace('>', ')')
            string = re.sub(r'(?P<single>[0-9]+\.[0-9]+)[eE]',
                           '\g<single>D',
                           string)
            mech_rates_list.append(
                'p(' + str(i) + ') = ' + string + '  !' + reaction_definitions[rate_counter])
            i += 1

    # Write RO2 data to file
    in_RO2_lines = False
    ro2List = []
    for item in peroxy_radicals:
        if not re.match('\*', item):
            # We have an equals sign on the first line. Handle this by splitting against =, then taking the last element of the
            # resulting list, which will either be the right-hand side of the first line, or the whole of any other line.
            # Similarly, the final line will end with a colon. Handle in a similar way.
            # Then split by +. Append each item to ro2_input: multiple appends use 'extend'
            ro2List.extend([elem.strip() for elem in item.split('=')[-1].split(';')[0].strip().split('+')])
    # Remove empty strings
    ro2List = filter(None, ro2List)

    # check RO2s are in RO2 list
    # Read in RO2 list, and strip off newlines
    with open(os.path.join(mcm_dir, 'peroxy-radicals_v3.3.1')) as RO2List_file:
        RO2List_input = [r.rstrip() for r in RO2List_file.readlines()]

    # Check that each species is in the RO2 list. If so, just print to screen. Otherwise, print a warning at the top of
    # mechanism-rate-coefficients.f90 for each errant species.
    print 'looping over inputted ro2s'

    with open(os.path.join(output_dir, 'mechanism-rate-coefficients.f90'), 'w') as mech_rates_file:
        mech_rates_file.write("""! Note that this file is generated by tools/mech_converter.py
! based upon the file tools/mcm_example.fac
! Any manual edits to this file will be overwritten when
! calling tools/mech_converter.py

""")
        for ro2_species in [element for  element in ro2List if element not in RO2List_input]:
            print ' ****** Warning: ' + ro2_species + ' NOT found in RO2List ****** '
            mech_rates_file.write('! ' + ro2_species +
                                  ' is not in the MCM list of RO2 species. Should it be in the RO2 sum?\n')

        # loop over RO2 and write the necessary line to mechanism-rate-coefficients.f90, using the species number of the RO2
        print 'adding RO2 to src/gen/ro2-rates.f90'
        with open(os.path.join(mc_dir, 'mechanism.ro2'), 'w') as ro2_file:
            ro2_file.write("""! Note that this file is generated by tools/mech_converter.py based upon the file tools/mcm_example.fac. Any manual edits to this file will be overwritten when calling tools/mech_converter.py
""")

            for ro2List_i in ro2List:
                for speciesNumber, y in zip(range(1, len(speciesList) + 1), speciesList):
                    if ro2List_i.strip() == y.strip():
                        ro2_file.write(str(speciesNumber) + ' !' + ro2List_i.strip() + '\n')
                        # Exit loop early if species found
                        break
                        # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the RO2 being
                        # found in the species list
                else:
                    ro2_file.write('0 ! error RO2 not in mechanism: ' + ro2List_i + '\n')


    coeffSpeciesList = ['N2', 'O2', 'M', 'RH', 'H2O', 'DEC', 'BLH', 'DILUTE', 'JFAC', 'ROOFOPEN']
    reactionNumber = 0
    mechanism_rates_coeff_list = []
    for line in generic_rate_coefficients + complex_reactions:
        # Check for comments (beginning with a !), or blank lines
        if (re.match('!', line) is not None) | (line.isspace()):
            mechanism_rates_coeff_list.append(line)
        # Check for lines starting with either ; or *, and write these as comments
        elif (re.match(';', line) is not None) | (re.match('[*]', line) is not None):
            mechanism_rates_coeff_list.append('!' + line)
        # Otherwise assume all remaining lines are in the correct format, and so process them
        else:
            # This matches anything like @-dd.d and replaces with **(-dd.d). This uses (?<=@) as a lookbehind assertion,
            # then matches - and any combination of digits and decimal points. This replaces the negative number by its
            # bracketed version.
            # It also then converts all @ to ** etc.
            line2 = re.sub('(?<=@)-[0-9.]*',
                           '(\g<0>)',
                           line.replace(';', '').strip()
                           ).replace('@', '**')
            # Append _DP to the end of all digits that aren't followed by more digits or letters (targets a few too many)
            line2 = re.sub('[0-9]+(?![a-zA-Z0-9\.])',
                           '\g<0>_DP',
                           line2)
            # Undo the suffix _DP for any species names and for LOG10
            line2 = re.sub(r'\b(?P<speciesnames>[a-zA-Z][a-zA-Z0-9]*)_DP',
                           '\g<speciesnames>',
                           line2)
            # Undo the suffix _DP for any numbers like 1D7 or 2.3D-8
            line2 = re.sub(r'\b(?P<doubles1>[0-9][0-9\.]*)[dDeE](?P<doubles2>[+-]*[0-9]+)_DP',
                           '\g<doubles1>e\g<doubles2>_DP',
                           line2)
            # Add .0 to any literals that don't have a decimal place - this is necessary as it seems you can't use extended
            # precision on such a number - gfortran complains about an unknown integer kind, when it should really be a real kind
            line2 = re.sub(r'(?<![\.0-9+-dDeE])(?P<doubles>[0-9]+)_DP',
                           '\g<doubles>.0_DP',
                           line2)
            # Save the resulting string to mechanism_rates_coeff_list
            mechanism_rates_coeff_list.append(line2 + '\n')

            # Now we need to find the list of all species that are used in these equations, so we can declare them
            # at the top of the Fortran source file.

            # reactionNumber keeps track of the line we are processing
            reactionNumber += 1

            # strip whitespace, ; and %
            line = line.strip().strip('%;').strip()

            # split by the semi-colon : a[0] is reaction rate, a[1] is reaction equation
            a = line

            # Process the reaction: split by = into reactants and products
            reaction_parts = re.split('=', a)

            LHSList = reaction_parts[0]
            RHSList = reaction_parts[1]

            # Process each of reactants and products by splitting by +. Strip each at this stage.
            reactant = LHSList.strip()  # [item.strip() for item in re.split('[+]', LHSList)]
            products = RHSList.strip()  # [item.strip() for item in re.split('[+]', RHSList)]

            # Compare reactant against known species.
            if reactant not in coeffSpeciesList:
                # Add reactant to coeffSpeciesList, and add this number to
                # reactantNums to record this reaction.
                coeffSpeciesList.append(reactant)
                # print 'adding', reactant, 'to coeffSpeciesList'

            if not RHSList.isspace():
                # Compare each product against known species.
                # Replace all math characters and brackets with spaces, and split the remaining string by spaces.
                # Now, each string in the sublist will:
                # - start with a digit
                # - be a 'reserved word' i.e. LOG10, EXP, TEMP, PRESS
                # - otherwise, be a species
                RHSList_sub = [item.upper() for item in re.sub('[()\-+*@/]', ' ', RHSList).split(' ')]
                # Filter out nunbers, and spaces, and any reserved words, and any known species
                coeffSpeciesList.extend([x for x in RHSList_sub if (not re.match('[0-9]', x))
                                            and (not x == '')
                                            and (not any(x == reserved for reserved in ['EXP', 'TEMP', 'PRESS', 'LOG10', 'T']))
                                            and (not x in coeffSpeciesList)])

    # Recombine the species found into lines of 10 in the right format to declare them as Fortran variables.
    # Begin the first line as necessary
    newline = 'real(kind=DP) ::'
    mechanism_rates_decl = []
    # Loop over all species
    for i, item in zip(range(1, len(coeffSpeciesList) + 1), coeffSpeciesList):
        # Add the next species
        newline += ' ' + item.strip()
        # If it's the last species, then exit the loop with some extra newlines
        if i == len(coeffSpeciesList):
            mechanism_rates_decl.append(newline + '\n\n\n')
            continue
        # Otherwise, every tenth species gets rounded off with a newline and a prefix to the next line
        if i % 10 == 0:
            mechanism_rates_decl.append(newline + '\n')
            newline = 'real(kind=DP) ::'
        else:
            # If not, add a spacer
            newline += ','

    with open(os.path.join(output_dir, 'mechanism-rate-declarations.f90'), 'w') as mr3_file:
        mr3_file.write("""! Note that this file is generated by tools/mech_converter.py
! based upon the file tools/mcm_example.fac
! Any manual edits to this file will be overwritten when
! calling tools/mech_converter.py

""")
        for item in mechanism_rates_decl:
            mr3_file.write(item)

    # # Combine mechanism rates and RO2 sum files
    with open(os.path.join(output_dir, 'mechanism-rate-coefficients.f90'), 'a') as mech_rates_coeff_file:
        for item in mechanism_rates_coeff_list:
            mech_rates_coeff_file.write(item)
        # copy .ftemp to .f90
        # rs = mech_rates_list.readlines()
        for r in mech_rates_list:
            mech_rates_coeff_file.write(r)

    # os.remove(os.path.join(input_directory, 'mechanism-rate-coefficients.ftemp'))


def main():
    assert len(sys.argv) > 1, 'Please enter a filename as first argument, pointing to the mcm mechanism file.'
    input_filename = sys.argv[1]
    assert len(sys.argv) > 2, 'Please enter a directory as second argument, pointing to the directory containing source files of AtChem2.'
    output_directory = sys.argv[2]
    assert len(sys.argv) > 3, 'Please enter a directory as third argument, pointing to the directory for mechanism.species etc.'
    param_directory = sys.argv[3]
    convert(input_filename, output_directory, param_directory)

if __name__ == '__main__':
    main()
