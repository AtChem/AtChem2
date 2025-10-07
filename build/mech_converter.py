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

# -------------------------------------------------------------------- #
# This script converts a chemical mechanism file -- in FACSIMILE (.fac)
# or KPP (.kpp) format -- into the Fortran-compatible format used by
# AtChem2.  The script generates 5 files in the model configuration
# `include/` directory:
#
# - mechanism.species
# - mechanism.reac
# - mechanism.prod
# - mechanism.ro2
# - mechanism.f90
#
# Acknowledgements: B. Nelson, M. Newland, A. Mayhew
#
# ARGUMENTS:
#   1. path to the mechanism .fac file
#   2. path to the model configuration directory [default: model/include/]
#   3. path to the MCM data files directory [default: mcm/]
# -------------------------------------------------------------------- #
from __future__ import print_function
import os
import sys
import re
import fix_mechanism_fac
import kpp_conversion

reservedSpeciesList = ['N2', 'O2', 'M', 'RH', 'H2O', 'BLHEIGHT', 'DEC', 'JFAC',
                       'DILUTE', 'ROOF', 'ASA', 'RO2']
reservedOtherList = ['EXP', 'LOG10', 'TEMP', 'PRESS', 'J', 't']

# =========================== FUNCTIONS =========================== #


def tokenise_and_process(input_string, vars_dict):
    """
    This function takes in a single string, and a dictionary of known
    variables from previous lines, and returns the same string but
    with known variables replaced by a reference to their matching
    element in a vector q. This removes the dependence on potentially
    100+ named variables, and replaces them with a single vector.

    Args:
        input_string (str): a string which will be used as the basis
                            of the return string (new_rhs)
        vars_dict (dict): a dictionary containing all the known
                              variables up to this point

    Returns:
        new_rhs (str): a string based on input_string, but with references
                       to known variables replaced by references to elements
                       in vector q
    """

    assert isinstance(input_string, str), \
        'tokenise_and_process: input_string is not of type str: ' + str(input_string)
    assert isinstance(vars_dict, dict), \
        'tokenise_and_process: vars_dict is not of type dict: ' + str(vars_dict)

    # Generate start and end points of sections of symbols and nonsymbols
    symbol_regex = r'[()\-+*@/, ]+'
    nonsymbol_regex = r'[^()\-+*@/, ]+'

    list_of_symbol_starts = [m.start(0) for m in re.finditer(symbol_regex, input_string)]
    list_of_symbol_ends = [m.end(0) for m in re.finditer(symbol_regex, input_string)]
    list_of_nonsymbol_starts = [m.start(0) for m in re.finditer(nonsymbol_regex, input_string)]
    list_of_nonsymbol_ends = [m.end(0) for m in re.finditer(nonsymbol_regex, input_string)]
    new_rhs = ''

    # Now that the symbol/non-symbol sections are identified, we need to create
    # the new string by recombining the other sections in the right order, with
    # some replaced by q(i) syntax.
    #
    # Recombine the lists in the right order, but replace the  non-symbols that
    # aren't numbers, reserved words or reserved species (and thus must be new
    # species/intermediate values) with q(i) notation.
    #
    # Loop while there are any substrings left.
    while list_of_symbol_starts != [] or list_of_nonsymbol_starts != []:
        # We should use the next symbol if:
        #  1) both lists are non-empty and the symbols list has a lower first element, or
        #  2) only the symbols list has anything left in it.
        if ((list_of_symbol_starts != [] and list_of_nonsymbol_starts != []) \
            and list_of_symbol_starts[0] < list_of_nonsymbol_starts[0]) \
            or (list_of_symbol_starts != [] and list_of_nonsymbol_starts == []):
            # Add next symbol. Print the substring as-is.
            new_rhs += input_string[list_of_symbol_starts[0]:list_of_symbol_ends[0]]
            # Remove the indices of this substring from the lists.
            del list_of_symbol_starts[0]
            del list_of_symbol_ends[0]
        # Otherwise, if there are only non-symbols left, or if the non-symbols
        # list has a lower first element.
        else:
            # Add next non-symbol. Get the substring of interest.
            varname = input_string[list_of_nonsymbol_starts[0]:list_of_nonsymbol_ends[0]]
            # If it's not a number or a reserved word, it must be a variable,
            # so substitute with the relevant element from q.
            if not re.match(r'^[0-9]', varname) and varname not in reservedSpeciesList \
               and varname not in reservedOtherList:
                new_rhs += 'q(' + str(vars_dict[varname]) + ')'
            # Otherwise, just print the substring as-is.
            else:
                new_rhs += input_string[list_of_nonsymbol_starts[0]:list_of_nonsymbol_ends[0]]
            # Remove the indices of this substring from the lists.
            del list_of_nonsymbol_starts[0]
            del list_of_nonsymbol_ends[0]

    # Return the reconstructed string.
    return new_rhs

def separate_stoichiometry(input_species):
    """
    This function takes in a string of a species from the mechanism and
    separates the species name from any preceeding stoichiometric coefficient.
    This assumes that no species names will begin with a number.

    Args:
        input_species(str): a string containing a species name with a possible
                            stoichiometric coefficient (e.g. H2O2, 2H2O2, 2 H2O2, or 0.5H2O2)

    Returns:
        split_spec (tuple): a tuple of a float and a string. The first (float) is the stoichiometric
                            coefficient, and the second (string) is the species name.
    """

    #regex to match the potential coefficient and name sections of the input
    in_pat = re.compile(r'^ *(\d*\.?\d*) *([a-zA-Z_].*) *$')
    pat_match = in_pat.match(input_species)
    if pat_match:
        if pat_match[1]: #if there is a coefficient passed
            return (float(pat_match[1]), pat_match[2])
        else: #if there is no coefficient then output an assumed coefficient of 1
            return (1.0, pat_match[2])
    else:
        raise Exception(f"""Reaction species does not match the correct
                        format: '{input_species}'. Note that species names should
                        not begin with numerical characters.""")

def convert_to_fortran(input_file, conf_dir, mcm_vers):
    """
    This function converts a chemical mechanism file into the
    Fortran-compatible format used by the AtChem2 ODE solver. The
    function takes as input a file with the chemical mechanism (either
    a .fac or .kpp file), and generates 5 Fortran files (mechanism.*)
    in a given directory (mech_dir):

    * Each species and reaction in the chemical mechanism is assigned
      an ID number.

    * The equations defined in sections 'Generic Rate Coefficients'
      and 'Complex reactions' go to the `mechanism.f90` file with little
      more than formatting changes -- each line is replicated in full,
      with each named rate converted to an element in vector q.

    * The reaction rates defined in section 'Reaction definitions' go
      to the `mechanism.f90` file as elements of vector p.

    * The species involved as reactants (respectively products) in the
      reactions in section 'Reaction definitions' are split up into
      individual species, and the species and reactions ID numbers go
      to `mechanism.reac` (respectively `mechanism.prod`). Combining
      `mechanism.reac`, `mechanism.prod` and the last section of
      `mechanism.f90` gives the original information contained in
      section 'Reaction definitions' of the .fac file, but in a
      format that AtChem2 can parse.

    * The ID numbers and names of all species in the chemical
      mechanism go to the `mechanism.species` file.

    * The ID numbers and names of all RO2 species in section 'Peroxy
      radicals' go to the `mechanism.ro2` file.

    Args:
        input_file (str): relative or absolute reference to the .fac file
        mech_dir (str): relative or absolute reference to the directory where
                        the mechanism.* files will be created, and where
                        the environmentVariables.config file should be read from
                        By default it is: model/include/
        mcm_vers (str): relative or absolute reference to the directory containing
                        the reference list of RO2 species (`peroxy-radicals_v*`)
                        By default it is: mcm/
    """

    # Get the directory and filename of input_file, and check that they exist.
    input_directory = os.path.dirname(os.path.abspath(input_file))
    input_filename = os.path.basename(input_file)
    input_path = os.path.join(input_directory, input_filename)

    assert os.path.isfile(input_path), \
        'The input file ' + str(input_path) + ' does not exist.'
    print('Chemical mechanism file in:', input_directory)

    mech_dir = os.path.join(conf_dir, 'include/')

    # Check if the chemical mechanism file is in KPP format, in which case convert it
    # to FACSIMILE format (see documentation of `kpp_conversion.py` for more info)
    if input_filename.split('.')[-1] == 'kpp':
        input_fac = kpp_conversion.write_fac_file(input_path)
    else:
        input_fac = input_path

    # Check and fix the .fac file of any errant newlines (see documentation
    # of `fix_mechanism_fac.py` for more info).
    fix_mechanism_fac.fix_fac_full_file(input_fac)

    # Read in the input file.
    print('Reading input file')
    with open(input_fac, 'r') as input_mech:
        mechList = input_mech.readlines()

    # Split the lines into the following sections:
    # - Ignore everything up to Generic Rate Coefficients
    # - Generic Rate Coefficients
    # - Complex reactions
    # - Peroxy radicals
    # - Reaction definitions
    section_headers_indices = [0, 1, 2, 3]
    section_headers = ['Generic Rate Coefficients', 'Complex reactions',
                       'Peroxy radicals', 'Reaction definitions']
    generic_rate_coefficients = []
    complex_reactions = []
    peroxy_radicals = []
    reaction_definitions = []

    section = 0
    for line in mechList:
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
            assert section == 0, 'Error, section is not in [0,4]'

    # Convert peroxy_radicals to a list of strings, with each of the RO2 species from the RO2 sum
    # in the 'Peroxy radicals' section.
    ro2List = []
    for item in peroxy_radicals:
        if not re.match(r'\*', item):
            # We have an equals sign on the first line. Handle this by splitting against '=',
            # then taking the last element of the resulting list, which will either be the
            # right-hand side of the first line, or the whole of any other line. Similarly,
            # the final line will end with a colon. Handle in a similar way. Then split
            # by '+'. Append each item to ro2_input: multiple appends use 'extend'.
            ro2List.extend([elem.strip() for elem in \
                            item.split('=')[-1].split(';')[0].strip().split('+')])
    # Remove empty strings.
    ro2List = list(filter(None, ro2List))

    # Read in the reference list of RO2 species from the MCM (peroxy-radicals_v*).
    #
    # - the RO2 reference list is specific to each version of the MCM
    # - RO2 lists for versions v3.1, v3.2, v3.3.1 of the MCM are available
    # - change the filename if using a version of the MCM other than the default (v3.3.1)
    #
    # TODO: implement a different way to set the mcm version (see issue #297)
    with open(os.path.join(mcm_vers, 'peroxy-radicals_v3.3.1'), 'r') as RO2List_file:
        RO2List_reference = [r.rstrip() for r in RO2List_file.readlines()]

    # Check that each of the RO2s from 'Peroxy radicals' are present
    # in the RO2 reference list from the MCM. If not, print a warning
    # at the top of mechanism.f90 for each errant species.
    print('Looping over inputted RO2s')

    with open(os.path.join(mech_dir, 'mechanism.f90'), 'w') as mech_rates_file:
        mech_rates_file.write('! Note that this file is automatically generated by build/mech_converter.py -- Any manual edits to this file will be overwritten when calling build/mech_converter.py\n')

        for ro2_species in [element for  element in ro2List if element not in RO2List_reference]:
            print('\n\t!!! WARNING !!!')
            print('  The following species are not present in the RO2 reference list:\n    ' + ro2_species)
            print('  Should they be included in the RO2 sum?\n')
            mech_rates_file.write('! ' + ro2_species + ' is not in the RO2 reference list for this version of the MCM\n')

    # Check the DILUTE environment variable to identify whether dilution should be applied.
    dilute = False
    with open(conf_dir + '/environmentVariables.config') as env_var_file:
        environmentVariables = env_var_file.readlines()
        for x in environmentVariables:
            x = x.split()
            try:
                if x[1] == 'DILUTE' and x[2] != 'NOTUSED':
                    dilute = x[2]
            except IndexError:
                continue

    # -------------------------------------------------
    # Read in the names of user-defined custom rate functions and add them
    # to the list of reserved names so that they will be carried through the
    # rate definitions (in a similar manner to LOG10)
    with open(conf_dir + '/customRateFuncs.f90') as custom_func_file:
        func_def_pat = r'function +([a-zA-Z0-9_]*) *\('
        custom_func_names = re.findall(func_def_pat, custom_func_file.read(), re.I)

        for n in custom_func_names:
            reservedOtherList.append(n)

    # Initialise list, dictionary and a counter.
    mechanism_rates_coeff_list = []
    variablesDict = {}
    reactionNumber = 0

    # Process sections 1 and 2 ('Generic Rate Coefficients', 'Complex reactions'):
    # - copy comment lines across.
    # - other lines are reformatted to Fortran syntax, then their contents edited
    #   to convert individual rate names to elements in a vector q.
    for line in generic_rate_coefficients + complex_reactions:

        # Check for comments (beginning with a '!'), or blank lines.
        if (re.match(r'!', line) is not None) or (line.isspace()):
            mechanism_rates_coeff_list.append(line)
        # Check for lines starting with either ';' or '*', and write these as comments.
        elif (re.match(r';', line) is not None) or (re.match(r'[*]', line) is not None):
            mechanism_rates_coeff_list.append('!' + line)
        # Otherwise assume all remaining lines are in the correct format, and process them.
        else:
            reactionNumber += 1   # keep track of the line we are processing

            # Match anything like '@-dd.d' and replaces with '**(-dd.d)'.
            # Use '(?<=@)' as a lookbehind assertion, then matches any combination
            # of digits and decimal points. Replace the negative number by its
            # bracketed version. Also convert all '@' to '**' etc...
            line2 = re.sub(r'(?<=@)-[0-9.]*',
                           r'(\g<0>)',
                           line.replace(';', '').strip()
                           ).replace('@', '**')
            # Append `_DP` to the end of all digits that aren't followed
            # by more digits or letters (targets a few too many).
            line2 = re.sub(r'[0-9]+(?![a-zA-Z0-9\.])',
                           r'\g<0>_DP',
                           line2)
            # Undo the suffix `_DP` for any species names and for LOG10.
            line2 = re.sub(r'\b(?P<speciesnames>[a-zA-Z][a-zA-Z0-9]*)_DP',
                           r'\g<speciesnames>',
                           line2)
            # Undo the suffix `_DP` for any numbers like 1D7 or 2.3D-8.
            line2 = re.sub(r'\b(?P<doubles1>[0-9][0-9\.]*)[dDeE](?P<doubles2>[+-]*[0-9]+)_DP',
                           r'\g<doubles1>e\g<doubles2>_DP',
                           line2)
            # Add .0 to any literals that don't have a decimal place. This is
            # necessary as it seems you can't use extended precision on such a
            # number -- gfortran complains about an unknown integer kind, when
            # it should really be a real kind.
            line2 = re.sub(r'(?<![\.0-9+-dDeE])(?P<doubles>[0-9]+)_DP',
                           r'\g<doubles>.0_DP',
                           line2)

            # Strip whitespace, ';' and '%'.
            cleaned_line = line2.strip().strip('%;').strip()

            # Process the assignment: split by '=' into variable names
            # and values, then strip each.
            [lhs, rhs] = re.split(r'=', cleaned_line)

            variable_name = lhs.strip()
            value = rhs.strip()

            # TODO: check for duplicates
            variablesDict[variable_name] = reactionNumber

            # Replace any variables declared here with references to q: each new
            # variable is assigned to a new element of q.
            new_rhs = tokenise_and_process(value, variablesDict)

            # Save the resulting string to mechanism_rates_coeff_list.
            mechanism_rates_coeff_list.append('q('+str(variablesDict[variable_name]) + ') = ' \
                                              + new_rhs + '  !' + cleaned_line + '\n')

    # Save the number of such equations to be output to mechanism.{prod,reac}.
    numberOfGenericComplex = reactionNumber

    # -------------------------------------------------

    # Initialise a few variables.
    speciesList = []
    rateConstants = []
    reactionNumber = 0

    # Process 'Reaction definitions'. We process this before 'Peroxy radicals'
    # because that relies on SpeciesList, which is generated here.
    # - copy comment lines across
    # - other lines are split into their consituent parts:
    #   - rateConstants are the reaction rates; these are processed via
    #     reformatting and tokenisation to use the vector q where needed.
    #   - the reactants and products of each species are collected up, numbered
    #     as necessary, and their placements output to mechanism.{prod,reac,species}
    mech_reac_list = []
    mech_prod_list = []

    # Loop over all lines in the reaction_definitions section of the input file.
    for line in reaction_definitions:

        # Check for comments (beginning with a '!'), or blank lines.
        if (re.match(r'!', line) is not None) or (line.isspace()):
            rateConstants.append(line)
        # Check for lines starting with either ';' or '*', and write these as comments.
        elif (re.match(r';', line) is not None) or (re.match(r'[*]', line) is not None):
            rateConstants.append('!' + line)
        # Otherwise assume all remaining lines are in the correct format, and process them.
        else:
            reactionNumber += 1   # keep track of the line we are processing

            # Strip whitespace, ';' and '%'.
            line = line.strip().strip('%;').strip()

            # Split by ':' -- lhs is reaction rate, rhs is reaction equation.
            [lhs, rhs] = re.split(r':', line)

            # Add reaction rate to rateConstants.
            rateConstants.append(lhs)

            # Process the reaction: split by '=' into reactants and products.
            [reactantsList, productsList] = re.split(r'=', rhs)

            # Process each of reactants and products by splitting by '+'.
            # Strip each at this stage.
            reactants = [item.strip() for item in re.split(r'[+]', reactantsList)]
            products = [item.strip() for item in re.split(r'[+]', productsList)]

            # Ignore empty reactantsList.
            if not reactantsList.strip() == '':
                # Compare each reactant against known species and note the
                # stoichometric coefficients for each reactant.
                reactantNums = []
                reactantStoichs = []
                for x in reactants:
                    #split the reactant into the species name and the stoichometric coefficient
                    x_coeff, x_name = separate_stoichiometry(x)
                    # Add the stoichometric coefficient to reactantStoichs
                    reactantStoichs.append(x_coeff)
                    # If the reactant is a known species then add its number to reactantNums.
                    if x_name in speciesList:
                        reactantNums.append(speciesList.index(x_name)+1)
                    else:
                        # Reactant x is not a known species. Add reactant to speciesList,
                        # then add this number to reactantNums to record this reaction.
                        speciesList.append(x_name)
                        reactantNums.append(len(speciesList))

                # Write the reactants to mech_reac_list.
                mech_reac_list.extend([f'{reactionNumber} {z} {y}\n' for \
                                       y,z in zip(reactantStoichs, reactantNums)])

            # Ignore empty productsList.
            if not productsList.strip() == '':
                # Compare each product against known species and note the
                # stoichometric coefficients for each product.
                productNums = []
                productStoichs = []
                for x in products:
                    #split the product into the species name and the stoichometric coefficient
                    x_coeff, x_name = separate_stoichiometry(x)
                    # Add the stoichometric coefficient to productStoichs
                    productStoichs.append(x_coeff)

                    # If the reactant is a known species then add its number to reactantNums.
                    if x_name in speciesList:
                        productNums.append(speciesList.index(x_name)+1)
                    else:
                        # Product x is not a known species. Add product to speciesList,
                        # then add this number to productNums to record this reaction.
                        speciesList.append(x_name)
                        productNums.append(len(speciesList))

                # Write the products to mech_prod_list.
                mech_prod_list.extend([f'{reactionNumber} {z} {y}\n' for \
                                       y,z in zip(productStoichs, productNums)])

    # -------------------------------------------------

    # Write out species for reactions to apply dilution factor if DILUTE is not NOTUSED.
    if dilute:
        for spec in speciesList:
            reactionNumber += 1
            mech_reac_list.append(str(reactionNumber) + ' ' \
                                  + str(speciesList.index(spec) + 1) + ' 1.0\n')

    with open(os.path.join(mech_dir, 'mechanism.prod'), 'w') as prod_file:
        # Output number of species and number of reactions.
        prod_file.write(str(len(speciesList)) + ' ' + str(reactionNumber) \
                        + ' ' + str(numberOfGenericComplex) \
                        + ' numberOfSpecies numberOfReactions numberOfGenericComplex\n')
        # Write all other lines.
        for line in mech_prod_list:
            prod_file.write(line)

    with open(os.path.join(mech_dir, 'mechanism.reac'), 'w') as reac_file:
        # Output number of species and number of reactions.
        reac_file.write(str(len(speciesList)) + ' ' + str(reactionNumber) \
                        + ' ' + str(numberOfGenericComplex) \
                        + ' numberOfSpecies numberOfReactions numberOfGenericComplex\n')
        # Write all other lines.
        for line in mech_reac_list:
            reac_file.write(line)

    # Write speciesList to mechanism.species, indexed by 1 to len(speciesList).
    with open(os.path.join(mech_dir, 'mechanism.species'), 'w') as species_file:
        for i, x in zip(range(1, len(speciesList) + 1), speciesList):
            species_file.write(str(i) + ' ' + str(x) + '\n')

    # Write out the rate coefficients.
    i = 0
    mech_rates_list = []
    for rate_counter, x in zip(range(len(mechList)), rateConstants):
        if (re.match(r'!', x) is not None) | (x.isspace()):
            mech_rates_list.append(str(x))
        else:
            # Match anything like '@-dd.d' and replaces with '**(-dd.d)'.
            # Use '(?<=@)' as a lookbehind assertion, then matches any combination
            # of digits and decimal points. Replace the negative number by its
            # bracketed version.
            i += 1
            string = re.sub(r'(?<=@)-[0-9.]*', r'(\g<0>)', x)
            # Now convert all '@' to '**' etc...
            string = string.replace('@', '**')
            string = string.replace('<', '(')
            string = string.replace('>', ')')
            # Replace any float-type numbers (xxx.xxxE+xx)
            # with a double-type number (xxx.xxxD+xx)
            string = re.sub(r'(?P<single>[0-9]+\.[0-9]+)[eE]',
                            r'\g<single>D',
                           string)
            mech_rates_list.append('p(' + str(i) + ') = ' \
                                   + tokenise_and_process(string, variablesDict) \
                                   + '  !' + reaction_definitions[rate_counter])

    # Write out further reactions to implement the dilution factor.
    if dilute:
        for _ in speciesList:
            i += 1
            mech_rates_list.append('p(' + str(i) + ') = DILUTE ! DILUTE\n')

    # -------------------------------------------------

    # Combine mechanism rates and RO2 sum files.
    with open(os.path.join(mech_dir, 'mechanism.f90'), 'a') as mech_rates_coeff_file:
        mech_rates_coeff_file.write("""
module mechanism_mod
    use, intrinsic :: iso_c_binding
    implicit none

contains

    subroutine update_p(p, q, t, TEMP, N2, O2, M, RH, H2O, BLHEIGHT, DEC, JFAC, DILUTE, ROOFOPEN, ASA, J, RO2) bind(c,name='update_p')

        use custom_functions_mod
        integer, parameter :: DP = selected_real_kind( p = 15, r = 307 )
        real(c_double), intent(inout) :: p(*), q(*)
        real(c_double), intent(in) :: t, TEMP, N2, O2, M, RH, H2O, BLHEIGHT, DEC, JFAC, DILUTE, ROOFOPEN, ASA, J(*), RO2
        """)

        # Write out 'Generic Rate Coefficients' and 'Complex reactions'.
        for item in mechanism_rates_coeff_list:
            mech_rates_coeff_file.write(item)

        # Write out 'Reaction definitions'.
        for r in mech_rates_list:
            mech_rates_coeff_file.write(r)
        mech_rates_coeff_file.write("""
    end subroutine update_p
end module mechanism_mod
""")

    # -------------------------------------------------

    # Finally, now that we have the full list of species, we can output the RO2s to
    # mechanism.ro2: loop over RO2 and write the necessary line to mechanism.ro2,
    # using the species ID number of the RO2.
    print('Adding RO2s to: ' + mech_dir + '/mechanism.ro2')
    with open(os.path.join(mech_dir, 'mechanism.ro2'), 'w') as ro2_file:
        ro2_file.write('! Note that this file is automatically generated by build/mech_converter.py -- Any manual edits to this file will be overwritten when calling build/mech_converter.py\n')

        for ro2List_i in ro2List:
            for speciesNumber, y in zip(range(1, len(speciesList) + 1), speciesList):
                if ro2List_i.strip() == y.strip():
                    ro2_file.write(str(speciesNumber) + ' !' + ro2List_i.strip() + '\n')
                    # Exit loop early if species found.
                    break
            # This code only executes if the break is NOT called, i.e. if the
            # loop runs to completion without the RO2 being found in the speciesList.
            else:
                error_message = ''.join([
                  ' ****** ',
                  'Error: RO2 species "',
                  str(ro2List_i.strip()),
                  '" NOT found in the mechanism. Please check the RO2 section',
                  ' of your mechanism file for incorrect species names!',
                  ' ******'])
                raise RuntimeError(error_message)


# =========================== MAIN =========================== #


def main():
    print('Processing chemical mechanism...')
    assert len(sys.argv) > 1, \
        'Enter the filename of a chemical mechanism (.fac or .kpp) as argument:'
    mech_file = sys.argv[1]
    # config_dir defaults to model/include/, if not given as argument
    if len(sys.argv) <= 2:
        config_dir = './model/include/'
    else:
        config_dir = sys.argv[2]
    # mcm_dir defaults to mcm/, if not given as argument
    if len(sys.argv) <= 3:
        mcm_dir = './mcm/'
    else:
        mcm_dir = sys.argv[3]

    # Check that the files and directories exist
    assert os.path.isfile(mech_file), 'Failed to find file ' + mech_file
    assert os.path.exists(config_dir), 'Failed to find directory ' + config_dir
    assert os.path.exists(mcm_dir), 'Failed to find directory ' + mcm_dir

    # Call the conversion to Fortran function
    convert_to_fortran(mech_file, config_dir, mcm_dir)
    print('... chemical mechanism converted to Fortran.')

# Call the main function if executed as script
if __name__ == '__main__':
    try:
        main()
    except RuntimeError as e:
        print(str(e))
        sys.exit(os.EX_DATAERR)
