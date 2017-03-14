import re
import os

f = open('./mechanism.fac')
reac = open('./mechanism.reactemp', 'w')
prod = open('./mechanism.prod', 'w')
species = open('./mechanism.species', 'w')
mechRates = open('./mechanism-rate-coefficients.ftemp', 'w')

s = f.readlines()
print s

speciesList = []
rateConstants = []
reactionNumber = 0

# Loop over all lines in the input file
for line in s:

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
        reaction = a[1]

        print 'reaction = ', reaction

        reaction_parts = re.split('=', reaction)
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
                # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the
                # reactant being found in the known species
                else:
                    # Add reactant to speciesList, and add this number to
                    # reactantNums to record this reaction.
                    speciesList.append(x)
                    reactantNums.append(len(speciesList))
                    print 'adding ', x, ' to speciesList'

            # Write the reactants to mechanism.reactemp
            for z in reactantNums:
                reac.write(str(reactionNumber) + ' ' + str(z) + '\n')

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
                # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the
                # product being found in the known species
                else:
                    # Add product to speciesList, add this number to
                    # productNums to record this reaction.
                    speciesList.append(x)
                    productNums.append(len(speciesList))
                    print 'adding ', x, ' to speciesList'

            # Write the products to mechanism.prod
            for z in productNums:
                prod.write(str(reactionNumber) + ' ' + str(z) + '\n')

# Mark end of file with zeros
reac.write('0\t0\t0\t0 \n')
prod.write('0\t0\t0\t0')
# Output number of species and number of reactions
st = str(len(speciesList)) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions\n'
reac.write(st)
reac.close()

# Copy mechanism.reactemp to mechanism.reac in a different order to make it readable by the model (move the last line to
# the first line).
reac1 = open('./mechanism.reactemp')
reacFin = open('./mechanism.reac', 'w')
st = reac1.readlines()
# Write last line
reacFin.write(st[len(st) - 1])
# Write all other lines
for line in st[:-1]:
    reacFin.write(line)

reac.close()
reacFin.close()

# Write speciesList to mechanism.species, indexed by (1 to len(speciesList))
for i, x in zip(range(1, len(speciesList)+1), speciesList):
    species.write(str(i) + ' ' + str(x) + '\n')

# Write out rate coefficients
i = 1
for rate_counter, x in zip(range(len(s)), rateConstants):
    if (re.match('!', x) is not None) | (x.isspace()):
        mechRates.write(str(x))
    else:
        string = x.replace('@', '**')
        string = string.replace('<', '(')
        string = string.replace('>', ')')
        mechRates.write('  p(' + str(i) + ') = ' + string + '  !' + s[rate_counter] + '\n')
        i += 1

mechRates.close()

fortranFile = open('./mechanism-rate-coefficients.f90', 'w')

# Read in RO2 data, which is arrange as 'RO2 = blah + blah + blah \n + blah + blah ;'
roFac = open('./RO2.fac')
ro2 = roFac.readlines()

ro2List = []
for l in ro2:
    # We have an equals sign on the first line. Handle this by splitting against =, then taking the last element of the
    # resulting list, which will either be the right-hand side of the first line, or the whole of any other line.
    l2 = l.split('=')[-1]
    # Then split by +
    strArray = l2.split('+')

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
fullRO2List = open('./RO2List')
fRO2l = fullRO2List.readlines()

for r in fRO2l:
    r = r.strip()
    print r

# Check that each species is in the RO2 list. If so, just print to screen. Otherwise, print a warning at the top of
# mechanism-rate-coefficients.f90 for each errant species.
print 'looping over inputted ro2s'
print 'The RO2List is: ', ro2List
for ro2i in ro2List:
    for ro2j in fRO2l:
        if ro2i.strip() == ro2j.strip():
            print ro2i.strip() + ' found in RO2List'
            break
    # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the species being
    # found in the RO2 list
    else:
        print ' ****** Warning: ' + ro2i.strip() + ' NOT found in RO2List ****** '
        fortranFile.write('! ' + ro2i.strip() + ' is not in the MCM list of RO2 species. Should it be in the RO2 sum?\n')

# loop over RO2 and write the necessary line to mechanism-rate-coefficients.f90, using the species number of the RO2
fortranFile.write('  ro2 = 0.00e+00\n')
for ro2i in ro2List:
    print 'ro2i: ' + ro2i
    for speciesNumber, y in zip(range(1, len(speciesList)+1), speciesList):
        if ro2i.strip() == y.strip():
            fortranFile.write('  ro2 = ro2 + y(' + str(speciesNumber) + ')!' + ro2i.strip() + '\n')
            # Exit loop early if species found
            break
    # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the RO2 being
    # found in the species list
    else:
        fortranFile.write('\t ! error RO2 not in mechanism: ' + ro2i + '\n')

fortranFile.write('\n\n')
# Read in NOY data, which is arrange as 'NOY = blah + blah + blah \n + blah + blah ;'
# NOYFac = open('./NOY.fac')
# NOY = NOYFac.readlines()
#
# NOYList = []
# for n in NOY:
# # We have an equals sign on the first line. Handle this by splitting against =, then taking the last element of the
# # resulting list, which will either be the right-hand side of the first line, or the whole of any other line.
#   n2 = n.split('=')[-1]
# # Then split by +
#	strArray = n.split('+')
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
# fortranFile.write('\tNOY = 0.00e+00\n')
# for NOYi in NOYList:
#	print 'NOYi: ' + NOYi
#	for speciesNumber, y in zip(range(1, len(speciesList)+1), speciesList):
#		if NOYi.strip() == y.strip():
#			fortranFile.write('  NOY = NOY + y(' + str(speciesNumber) + ')!' + NOYi.strip() + '\n')
#			# Exit loop early if species found
#		    break
# # This code only executes if the break is NOT called, i.e. if the loop runs to completion without the NOY being
# # found in the species list
#   else:
#		fortranFile.write('\t !error NOY not in mechanism: ' + NOYi + '\n')
#
# fortranFile.write('\n\n')
# # Combine mechanism rates and RO2 / NOY sum files
rates = open('./mechanism-rate-coefficients.ftemp')
rs = rates.readlines()

for r in rs:
    fortranFile.write(r)

os.remove('./mechanism.reactemp')
os.remove('./mechanism-rate-coefficients.ftemp')
