# This program fixes the input file of errant newlines, then output the reactants, products, species list, and rates.
# This only reads a file containing the 'reaction definitions' part.
import re
import os
import fix_mechanism_fac

# Fix the input contents of any errant newlines
fix_mechanism_fac.fix_fac_full_file('./mcm_subset.fac')

# Read in the input file
with open('./mcm_subset.fac') as input_file:
    s = input_file.readlines()
    print s

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
        print line
        generic_rate_coefficients.append(line)
    elif section == 2:
        print line
        complex_reactions.append(line)
    elif section == 3:
        print line
        peroxy_radicals.append(line)
    elif section == 4:
        print line
        reaction_definitions.append(line)
    else:
        assert section == 0, "Error, section is not in [0,4]"
        print line

print 'generic_rate_coefficients'
print generic_rate_coefficients
print 'complex_reactions'
print complex_reactions
print 'peroxy_radicals'
print peroxy_radicals
print 'reaction_definitions'
print reaction_definitions
# Initialise a few variables
speciesList = []
rateConstants = []
reactionNumber = 0

with open('./mechanism.reactemp', 'w') as reac_temp_file, open('./mechanism.prod', 'w') as prod_file:
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
            print 'line =', line
            # strip whitespace, ; and %
            line = line.strip()
            line = line.strip('[;]')
            line = line.strip('[%]')
            print 'line =', line
            # split by the semi-colon : a[0] is reaction rate, a[1] is reaction equation
            a = re.split(':', line)
            print 'a = ', a

            # Add reaction rate to rateConstants
            rateConstant = a[0]
            rateConstants.append(rateConstant)
            print rateConstant

            # Process the reaction: split by = into reactants and products
            print 'reaction = ', a[1]

            reaction_parts = re.split('=', a[1])
            print reaction_parts

            reactantsList = reaction_parts[0]
            productsList = reaction_parts[1]

            print 'reactantlist = ', reactantsList
            print 'productlist = ', productsList

            # Process each of reactants and products by splitting by +. Strip each at this stage.
            reactants = [item.strip() for item in re.split('[+]', reactantsList)]
            products = [item.strip() for item in re.split('[+]', productsList)]

            print 'reactants = ', reactants
            print 'products = ', products

            # Ignore empty reactantsList
            if not reactantsList.isspace():
                # Compare each reactant against known species.
                reactantNums = []
                for x in reactants:
                    j = 0
                    for y in speciesList:
                        # Check for equality: if equality, then the reactant is a known species and its number should be
                        # added to the reactantNums variable
                        if x == y:
                            reactantNums.append(j + 1)
                            print 'found: ', y, 'j = ', j
                            break
                        j += 1
                    # This code only executes if the break is NOT called, i.e. if the loop runs to completion without
                    # the reactant being found in the known species
                    else:
                        # Add reactant to speciesList, and add this number to
                        # reactantNums to record this reaction.
                        speciesList.append(x)
                        reactantNums.append(len(speciesList))
                        print 'adding ', x, ' to speciesList'

                # Write the reactants to mechanism.reactemp
                for z in reactantNums:
                    reac_temp_file.write(str(reactionNumber) + ' ' + str(z) + '\n')

            if not productsList.isspace():
                # Compare each product against known species.
                productNums = []
                for x in products:
                    j = 0
                    for y in speciesList:
                        # Check for equality: if equality, then the product is a known species and its number should be
                        # added to the productNums variable
                        if x == y:
                            productNums.append(j + 1)
                            print 'found: ', y, 'j = ', j
                            break
                        j += 1
                    # This code only executes if the break is NOT called, i.e. if the loop runs to completion without
                    # the product being found in the known species
                    else:
                        # Add product to speciesList, add this number to
                        # productNums to record this reaction.
                        speciesList.append(x)
                        productNums.append(len(speciesList))
                        print 'adding ', x, ' to speciesList'

                # Write the products to mechanism.prod
                for z in productNums:
                    prod_file.write(str(reactionNumber) + ' ' + str(z) + '\n')

    # Mark end of file with zeros
    reac_temp_file.write('0\t0\t0\t0 \n')
    prod_file.write('0\t0\t0\t0')
    # Output number of species and number of reactions
    reac_temp_file.write(str(len(speciesList)) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions\n')

# Copy mechanism.reactemp to mechanism.reac in a different order to make it readable by the model (move the last line to
# the first line).
with open('./mechanism.reactemp') as reac_temp_file, open('./mechanism.reac', 'w') as reac_file:
    st = reac_temp_file.readlines()
    # Write last line
    reac_file.write(st[len(st) - 1])
    # Write all other lines
    for line in st[:-1]:
        reac_file.write(line)

# Write speciesList to mechanism.species, indexed by (1 to len(speciesList))
with open('./mechanism.species', 'w') as species_file:
    for i, x in zip(range(1, len(speciesList)+1), speciesList):
        species_file.write(str(i) + ' ' + str(x) + '\n')

# Write out rate coefficients
i = 1
with open('./mechanism-rate-coefficients.ftemp', 'w') as mech_rates_temp_file:
    for rate_counter, x in zip(range(len(s)), rateConstants):
        if (re.match('!', x) is not None) | (x.isspace()):
            mech_rates_temp_file.write(str(x))
        else:
            string = x.replace('@', '**')
            string = string.replace('<', '(')
            string = string.replace('>', ')')
            mech_rates_temp_file.write('  p(' + str(i) + ') = ' + string + '  !' + reaction_definitions[rate_counter])
            i += 1

# Write RO2 data to file
in_RO2_lines = False
with open('./RO2.fac', 'w') as ro2_fac_file:
    for item in peroxy_radicals:
        if not in_RO2_lines:
            # Check to see whether we are entering the 'Reaction definitions' section
            if 'RO2 = ' in item:
                in_RO2_lines = True
        if in_RO2_lines:
            if not re.match('\*', item):
                ro2_fac_file.write(item)
            else:
                in_RO2_lines = False

# Read in RO2 data, which is arrange as 'RO2 = blah + blah + blah \n + blah + blah ;'
with open('./RO2.fac') as ro2_fac_file:
    ro2_input = ro2_fac_file.readlines()

ro2List = []
for l in ro2_input:
    # We have an equals sign on the first line. Handle this by splitting against =, then taking the last element of the
    # resulting list, which will either be the right-hand side of the first line, or the whole of any other line.
    # Then split by +.
    strArray = l.split('=')[-1].split('+')

    print strArray
    # For each element, remove any semi-colons, strip, and then append if non-empty.
    for x in strArray:
        x = x.replace(';', '').strip()
        if x == '':
            print 'doing nothing'
        else:
            print x
            ro2List.append(x)

# check RO2s are in RO2 list
with open('./RO2List') as RO2List_file:
    RO2List_input = RO2List_file.readlines()

for r in RO2List_input:
    r = r.strip()
    print r

# Check that each species is in the RO2 list. If so, just print to screen. Otherwise, print a warning at the top of
# mechanism-rate-coefficients.f90 for each errant species.
print 'looping over inputted ro2s'
print 'The RO2List is: ', ro2List

with open('./mechanism-rate-coefficients.f90', 'w') as mech_rates_file:
    for ro2List_i in ro2List:
        for ro2List_input_j in RO2List_input:
            if ro2List_i.strip() == ro2List_input_j.strip():
                print ro2List_i.strip() + ' found in RO2List'
                break
        # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the species
        # being found in the RO2 list
        else:
            print ' ****** Warning: ' + ro2List_i.strip() + ' NOT found in RO2List ****** '
            mech_rates_file.write('! ' + ro2List_i.strip() +
                                  ' is not in the MCM list of RO2 species. Should it be in the RO2 sum?\n')

    # loop over RO2 and write the necessary line to mechanism-rate-coefficients.f90, using the species number of the RO2
    mech_rates_file.write('  ro2 = 0.00e+00\n')
    for ro2List_i in ro2List:
        print 'ro2List_i: ' + ro2List_i
        for speciesNumber, y in zip(range(1, len(speciesList)+1), speciesList):
            if ro2List_i.strip() == y.strip():
                mech_rates_file.write('  ro2 = ro2 + y(' + str(speciesNumber) + ')!' + ro2List_i.strip() + '\n')
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
    # # Combine mechanism rates and RO2 / NOY sum files
    with open('./mechanism-rate-coefficients.ftemp') as mech_rates_temp_file:
        rs = mech_rates_temp_file.readlines()

    for r in rs:
        mech_rates_file.write(r)

os.remove('./mechanism.reactemp')
os.remove('./mechanism-rate-coefficients.ftemp')

mechanism_rates_list = ["""subroutine mechanism_rates(p,t,y,mnsp)
    USE photolysisRates
    USE zenithData1
    USE constraints
  use envVars, only: ro2

    implicit none

    ! calculates rate constants from arrhenius informtion
    double precision, intent(out) :: p(*)
    double precision, intent(in) :: t
    integer, intent(in) :: mnsp
    double precision, intent(in) :: y(mnsp)
    double precision:: kro2no3, temp
    double precision:: m, o2, n2, h2o, k0, ki, fc, f, k1, k2, k3, k4, kmt01, kmt02
    double precision:: kmt03, kmt04, kmt05, kmt06, kmt07, kmt08, kmt09,kmt10,kmt11
    double precision:: kmt12, kmt13, kmt14, kmt15, kmt16, kmt17, kfpan, kbpan
    double precision:: fcc,krc,fcd,krd,fc2,fc1,fc3,fc4,kr1,kr2,kr3,kr4
    double precision:: fc7,fc8,fc9,fc10,fc13,fc14,kr7,kr8,kr9,kr10,kr13,kr14
    double precision:: kc0, kci,kd0,kdi,fd,k10,k1i,f1,k20,k2i,k30,k3i,f3
    double precision:: k40,k4i,k70,k7i,f7,k80,k8i,f8
    double precision:: k90,k9i,f9,k100,k10i,f10,k130,k13i,f13,k140,k14i,f14
    double precision:: k160,k16i,kr16,fc16,f16
    double precision:: RH,dilute,jfac,roofOpen

    ! delcare variables missed in MCM definition
    double precision:: kroprim,krosec,kdec,kno3al,kapno,kapho2, K298CH3O2, KCH3O2
    double precision:: K14ISOM1, NCD, NC, NC1, NC2, NC3, NC4, NC7, NC8, NC9, NC10, NC12, NC13, NC14, NC15, NC16, NC17
    double precision:: F2, F4, F12, F15, F17
    double precision:: K120, K12I, KR12, FC12
    double precision:: K150, K15I, KR15, FC15
    double precision:: K170, K17I, KR17, FC17
    double precision:: KMT18, KPPN0, KPPNI, KRPPN, FCPPN, NCPPN, FPPN, KBPPN
    double precision :: kro2ho2,kro2no, fa2,fa4
    integer :: i
    double precision :: dec
    double precision::  photoRateAtT
    double precision:: blh, pressure, dummy

  include 'modelConfiguration/mechanism-rate-declarations.f90'

    call ro2sum(ro2, y)
    dummy = y(1)

    dec = -1e16

    call getEnvVarsAtT(t,temp,rh,h2o,dec,pressure,m,blh,dilute,jfac,roofOpen)

  call atmosphere(O2, N2,m)




    !O2 = 0.2095*m
    !N2 = 0.7809*m



    ! * **** SIMPLE RATE COEFFICIENTS *****                     *"""]

for item in generic_rate_coefficients:
    if re.match('\*', item):
        item = '!' + item
    mechanism_rates_list.append('    ' + item.replace(';', '').strip() + '\n')
for item in complex_reactions:
    if re.match('\*', item):
        item = '!' + item
    mechanism_rates_list.append('    ' + item.replace(';', '').strip().replace('@', '**') + '\n')
mechanism_rates_list.append("""    do i = 1, nrOfPhotoRates
        if (useConstantValues .eq. 0) then
            if (cosx.lt.1.00d-10) then
                j(ck(i)) = 1.0d-30
            else
                j(ck(i)) = cl(i)*cosx**( cmm(i))*exp(-cnn(i)*secx)*transmissionFactor(i)*roofOpen*jfac
            endif
        else
            j(ck(i)) = cl(i)
        endif
    enddo

    do  i =1,numConPhotoRates
        call getConstrainedQuantAtT2D(t,photoX,photoY,photoY2,photoNumberOfPoints(i),photoRateAtT, 2,i, &
            maxNumberOfDataPoints,numConPhotoRates)
        j(constrainedPhotoRatesNumbers(i)) = photoRateAtT
    enddo

    include 'modelConfiguration/mechanism-rate-coefficients.f90'
    return
end

include 'modelConfiguration/extraOutputSubroutines.f90'
""")
with open('../mechanism-rates2.f90', 'w+') as mr2_file:
    for item in mechanism_rates_list:
        mr2_file.write(item)
