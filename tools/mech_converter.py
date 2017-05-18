# This program fixes the input file of errant newlines, then output the reactants, products, species list, and rates.
# This only reads a file containing the 'reaction definitions' part.
import re
import os
import sys
import fix_mechanism_fac


def convert(input_file, output_dir):
    script_directory = os.path.dirname(os.path.abspath(__file__))
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
        # print s

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
            # print line
            generic_rate_coefficients.append(line)
        elif section == 2:
            # print line
            complex_reactions.append(line)
        elif section == 3:
            # print line
            peroxy_radicals.append(line)
        elif section == 4:
            # print line
            reaction_definitions.append(line)
        else:
            assert section == 0, "Error, section is not in [0,4]"
            # print line

    # print 'generic_rate_coefficients'
    # print generic_rate_coefficients
    # print 'complex_reactions'
    # print complex_reactions
    # print 'peroxy_radicals'
    # print peroxy_radicals
    # print 'reaction_definitions'
    # print reaction_definitions
    # Initialise a few variables
    speciesList = []
    rateConstants = []
    reactionNumber = 0

    with open(os.path.join(input_directory, 'mechanism.reactemp'), 'w') as reac_temp_file, open(
            os.path.join(input_directory, 'mechanism.prod'), 'w') as prod_file:
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
                # print 'line =', line
                # strip whitespace, ; and %
                line = line.strip().strip('%').strip(';').strip()

                print ''
                print 'line =', line
                # split by the semi-colon : a[0] is reaction rate, a[1] is reaction equation
                a = re.split(':', line)
                # print 'a = ', a

                # Add reaction rate to rateConstants
                rateConstants.append(a[0])
                # print 'rate =', a[0]

                # Process the reaction: split by = into reactants and products
                # print 'reaction = ', a[1]

                reaction_parts = re.split('=', a[1])
                # print reaction_parts

                reactantsList = reaction_parts[0]
                productsList = reaction_parts[1]

                # print 'reactantlist = ', reactantsList
                # print 'productlist = ', productsList

                # Process each of reactants and products by splitting by +. Strip each at this stage.
                reactants = [item.strip() for item in re.split('[+]', reactantsList)]
                products = [item.strip() for item in re.split('[+]', productsList)]

                # print 'reactants =', reactants
                # print 'products =', products

                # Ignore empty reactantsList
                if not reactantsList == '':
                    # Compare each reactant against known species.
                    reactantNums = []
                    for x in reactants:
                        j = 0
                        for y in speciesList:
                            # Check for equality: if equality, then the reactant is a known species and its number should be
                            # added to the reactantNums variable
                            if x == y:
                                reactantNums.append(j + 1)
                                # print 'found:', y + ', j =', j
                                break
                            j += 1
                        # This code only executes if the break is NOT called, i.e. if the loop runs to completion without
                        # the reactant being found in the known species
                        else:
                            # Add reactant to speciesList, and add this number to
                            # reactantNums to record this reaction.
                            speciesList.append(x)
                            reactantNums.append(len(speciesList))
                            print 'adding', x, 'to speciesList'

                    # Write the reactants to mechanism.reactemp
                    for z in reactantNums:
                        reac_temp_file.write(str(reactionNumber) + ' ' + str(z) + '\n')

                if not productsList == '':
                    # Compare each product against known species.
                    productNums = []
                    for x in products:
                        j = 0
                        for y in speciesList:
                            # Check for equality: if equality, then the product is a known species and its number should be
                            # added to the productNums variable
                            if x == y:
                                productNums.append(j + 1)
                                # print 'found:', y + ', j =', j
                                break
                            j += 1
                        # This code only executes if the break is NOT called, i.e. if the loop runs to completion without
                        # the product being found in the known species
                        else:
                            # Add product to speciesList, add this number to
                            # productNums to record this reaction.
                            speciesList.append(x)
                            productNums.append(len(speciesList))
                            print 'adding', x, 'to speciesList'

                    # Write the products to mechanism.prod
                    for z in productNums:
                        prod_file.write(str(reactionNumber) + ' ' + str(z) + '\n')

        # Output number of species and number of reactions
        reac_temp_file.write(str(len(speciesList)) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions\n')

    # Copy mechanism.reactemp to mechanism.reac in a different order to make it readable by the model (move the last line to
    # the first line).
    with open(os.path.join(input_directory, 'mechanism.reactemp')) as reac_temp_file, open(
            os.path.join(input_directory, 'mechanism.reac'), 'w') as reac_file:
        st = reac_temp_file.readlines()
        # Write last line
        reac_file.write(st[len(st) - 1])
        # Write all other lines
        for line in st[:-1]:
            reac_file.write(line)

    # Write speciesList to mechanism.species, indexed by (1 to len(speciesList))
    with open(os.path.join(input_directory, 'mechanism.species'), 'w') as species_file:
        for i, x in zip(range(1, len(speciesList) + 1), speciesList):
            species_file.write(str(i) + ' ' + str(x) + '\n')

    # Write out rate coefficients
    i = 1
    with open(os.path.join(input_directory, 'mechanism-rate-coefficients.ftemp'), 'w') as mech_rates_temp_file:
        for rate_counter, x in zip(range(len(s)), rateConstants):
            if (re.match('!', x) is not None) | (x.isspace()):
                mech_rates_temp_file.write(str(x))
            else:
                # This matches anything like @-dd.d and replaces with **(-dd.d). This uses (?<=@) as a lookbehind assertion,
                # then matches - and any combination of digits and decimal points. This replaces the negative number by its
                # bracketed version.
                string = re.sub('(?<=@)-[0-9.]*', '(\g<0>)', x)
                # Now convert all @ to ** etc.
                string = string.replace('@', '**')
                string = string.replace('<', '(')
                string = string.replace('>', ')')
                mech_rates_temp_file.write(
                    'p(' + str(i) + ') = ' + string + '  !' + reaction_definitions[rate_counter])
                i += 1

    # Write RO2 data to file
    in_RO2_lines = False
    ro2_input = []
    for item in peroxy_radicals:
        if not in_RO2_lines:
            # Check to see whether we are entering the 'Reaction definitions' section
            if 'RO2 = ' in item:
                in_RO2_lines = True
        if in_RO2_lines:
            if not re.match('\*', item):
                ro2_input.append(item)
            else:
                in_RO2_lines = False

    ro2List = []
    for l in ro2_input:
        # We have an equals sign on the first line. Handle this by splitting against =, then taking the last element of the
        # resulting list, which will either be the right-hand side of the first line, or the whole of any other line.
        # Then split by +.
        strArray = l.split('=')[-1].split('+')

        # print strArray
        # For each element, remove any semi-colons, strip, and then append if non-empty.
        for x in strArray:
            x = x.replace(';', '').strip()
            if x == '':
                pass
                # print 'doing nothing'
            else:
                # print x
                ro2List.append(x)

    # check RO2s are in RO2 list
    with open(os.path.join(script_directory, 'RO2listv3.3.1')) as RO2List_file:
        RO2List_input = RO2List_file.readlines()

    for r in RO2List_input:
        r = r.strip()
        # print r

    # Check that each species is in the RO2 list. If so, just print to screen. Otherwise, print a warning at the top of
    # mechanism-rate-coefficients.f90 for each errant species.
    print 'looping over inputted ro2s'
    # print 'The RO2List is: ', ro2List

    with open(os.path.join(output_dir, 'mechanism-rate-coefficients.f90'), 'w') as mech_rates_file:
        mech_rates_file.write("""! Note that this file is generated by tools/mech_converter.py,
! based upon the file tools/mcm_subset.fac. Any manual edits to this file will be overwritten
! when calling tools/mech_converter.py

""")
        for ro2List_i in ro2List:
            for ro2List_input_j in RO2List_input:
                if ro2List_i.strip() == ro2List_input_j.strip():
                    # print ro2List_i.strip() + ' found in RO2List'
                    break
            # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the species
            # being found in the RO2 list
            else:
                print ' ****** Warning: ' + ro2List_i.strip() + ' NOT found in RO2List ****** '
                mech_rates_file.write('! ' + ro2List_i.strip() +
                                      ' is not in the MCM list of RO2 species. Should it be in the RO2 sum?\n')

        # loop over RO2 and write the necessary line to mechanism-rate-coefficients.f90, using the species number of the RO2
        mech_rates_file.write('ro2 = 0.00e+00\n')
        print 'adding RO2 to mechanism-rate-coefficients.f90'
        for ro2List_i in ro2List:
            # print 'ro2List_i: ' + ro2List_i
            for speciesNumber, y in zip(range(1, len(speciesList) + 1), speciesList):
                if ro2List_i.strip() == y.strip():
                    mech_rates_file.write('ro2 = ro2 + y(' + str(speciesNumber) + ')!' + ro2List_i.strip() + '\n')
                    # Exit loop early if species found
                    break
            # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the RO2 being
            # found in the species list
            else:
                mech_rates_file.write('\t ! error RO2 not in mechanism: ' + ro2List_i + '\n')

        mech_rates_file.write('\n\n')
        # Read in NOY data, which is arrange as 'NOY = blah + blah + blah \n + blah + blah ;'
        # with open('./NOY.fac') as NOY_fac_file
        #   NOY_input = NOY_fac_file.readlines()
        #
        # NOYList = []
        # for n in NOY_input:
        # # We have an equals sign on the first line. Handle this by splitting against =, then taking the last element of the
        # # resulting list, which will either be the right-hand side of the first line, or the whole of any other line.
        # # Then split by +
        #   strArray = n.split('=')[-1].split('+')
        #
        #	print strArray
        # # For each element, remove any semi-colons, strip, and then append if non-empty.
        #	for x in strArray:
        #		x = x.replace(';', '').strip()
        #		if x == '':
        #			print 'doing nothing'
        #		else:
        #			print x
        #			NOYList.append(x)
        #
        # # loop over NOY and write the necessary line to mechanism-rate-coefficients.f90, using the species number of the NOY
        # mech_rates_file.write('\tNOY = 0.00e+00\n')
        # for NOYList_i in NOYList:
        #	print 'NOYList_i: ' + NOYList_i
        #	for speciesNumber, y in zip(range(1, len(speciesList)+1), speciesList):
        #		if NOYList_i.strip() == y.strip():
        #			mech_rates_file.write('  NOY = NOY + y(' + str(speciesNumber) + ')!' + NOYList_i.strip() + '\n')
        #			# Exit loop early if species found
        #		    break
        # # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the NOY being
        # # found in the species list
        #   else:
        #		mech_rates_file.write('\t !error NOY not in mechanism: ' + NOYList_i + '\n')
        #
        # mech_rates_file.write('\n\n')


    os.remove(os.path.join(input_directory, 'mechanism.reactemp'))

    coeffSpeciesList = ['N2', 'O2', 'M', 'RH', 'H2O', 'DEC', 'BLH', 'DILUTE', 'JFAC', 'ROOFOPEN']
    reactionNumber = 0
    mechanism_rates_coeff_list = []
    # P
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
            # Save the resulting string to mechanism_rates_coeff_list
            mechanism_rates_coeff_list.append(re.sub('(?<=@)-[0-9.]*',
                                                     '(\g<0>)',
                                                     line.replace(';', '').strip()
                                                     ).replace('@', '**')
                                              + '\n')

            # Now we need to find the list of all species that are used in these equations, so we can declare them
            # at the top of the Fortran source file.

            # reactionNumber keeps track of the line we are processing
            reactionNumber += 1
            # print 'line =', line
            # strip whitespace, ; and %
            line = line.strip().strip('%').strip(';').strip()
            print 'line =', line
            # split by the semi-colon : a[0] is reaction rate, a[1] is reaction equation
            a = line
            # print 'a = ', a

            # Add reaction rate to rateConstants
            # rateConstant = a[0]
            # rateConstants.append(rateConstant)
            # print rateConstant

            # Process the reaction: split by = into reactants and products
            # print 'reaction =', a

            reaction_parts = re.split('=', a)
            # print reaction_parts

            LHSList = reaction_parts[0]
            RHSList = reaction_parts[1]

            # print 'reactantlist = ', LHSList
            # print 'productlist = ', RHSList

            # Process each of reactants and products by splitting by +. Strip each at this stage.
            reactant = LHSList.strip()  # [item.strip() for item in re.split('[+]', LHSList)]
            products = RHSList.strip()  # [item.strip() for item in re.split('[+]', RHSList)]

            # print 'reactants = ', reactants
            # print 'products = ', products

            # Compare reactant against known species.
            if reactant in coeffSpeciesList:
                pass
                # print 'found:', reactant
            else:
                # Add reactant to coeffSpeciesList, and add this number to
                # reactantNums to record this reaction.
                coeffSpeciesList.append(reactant)
                print 'adding', reactant, 'to coeffSpeciesList'

            if not RHSList.isspace():
                # Compare each product against known species.
                productNums = []
                # print RHSList
                # Replace all math characters and brackets with spaces, and split the remaining string by spaces.
                # Now, each string in the sublist will:
                # - start with a digit
                # - be a 'reserved word' i.e. LOG10, EXP, TEMP, PRESS
                # - otherwise, be a species
                RHSList_sub = re.sub('[()\-+*@/]', ' ', RHSList).split(' ')
                # print RHSList_sub
                RHSList_sub = [item.upper() for item in RHSList_sub]
                for x in RHSList_sub:
                    # Filter out spaces, numbers, and maths symbols
                    if (not re.match('[0-9]', x)) and (not x == ''):
                        # Filter out our 'reserved words'
                        if not any(x == reserved for reserved in ['EXP', 'TEMP', 'PRESS', 'LOG10', 'T']):
                            # print x
                            if x in coeffSpeciesList:
                                pass
                                # print 'found: ', x
                            else:
                                coeffSpeciesList.append(x)
                                print 'adding', x, 'to coeffSpeciesList'

    # Recombine the species found into lines of 10 in the right format to declare them as Fortran variables.
    # Begin wthe first line as necessary
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

    # print mechanism_rates_decl
    with open(os.path.join(output_dir, 'mechanism-rate-declarations.f90'), 'w') as mr3_file:
        mr3_file.write("""! Note that this file is generated by tools/mech_converter.py,
! based upon the file tools/mcm_subset.fac. Any manual edits to this file will be overwritten
! when calling tools/mech_converter.py

""")
        for item in mechanism_rates_decl:
            mr3_file.write(item)

    # # Combine mechanism rates and RO2 / NOY sum files
    with open(os.path.join(input_directory, 'mechanism-rate-coefficients.ftemp'), 'r') as mech_rates_temp_file, \
        open(os.path.join(output_dir, 'mechanism-rate-coefficients.f90'), 'a') as mech_rates_coeff_file:
        # print mechanism_rates_coeff_list
        for item in mechanism_rates_coeff_list:
            mech_rates_coeff_file.write(item)
        # copy .ftemp to .f90
        rs = mech_rates_temp_file.readlines()
        for r in rs:
            mech_rates_coeff_file.write(r)

    os.remove(os.path.join(input_directory, 'mechanism-rate-coefficients.ftemp'))


def main():
    assert len(sys.argv) > 1, 'Please enter a filename as first argument, pointing to the mcm subset file.'
    input_filename = sys.argv[1]
    assert len(sys.argv) > 2, 'Please enter a directory as second argument, pointing to the directory containing source files of AtChem.'
    output_directory = sys.argv[2]
    convert(input_filename, output_directory)

if __name__ == '__main__':
    main()
