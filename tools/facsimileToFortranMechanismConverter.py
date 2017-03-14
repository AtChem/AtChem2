import re
import os

f = open('./mechanism.fac')
reac = open('./mechanism.reactemp', 'w')
prod = open('./mechanism.prod', 'w')
species = open('./mechanism.species', 'w')
mechRates = open('./mechanism-rate-coefficients.ftemp', 'w')

s = f.readlines()
print s

speciesListCounter = 0
speciesList = []
rateConstants = []
reactionNumber = 0
reacFileCounter = 0

# Loop over all lines in the input file
for line in s:

    # Check for comments (beginning with a !)
    if re.match('!', line) is not None:
        rateConstants.append(line)
    # Check for blank lines
    elif line.isspace():
        rateConstants.append(line)
    # Check for lines starting with either ; or *
    elif (re.match(';', line) is not None) | (re.match('[*]', line) is not None):
        st = '!' + line
        rateConstants.append(st)
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
                speciesNumFound = False
                for y in speciesList:
                    # Check for equality: if equality, then the reactant is a known species and its number should be
                    # added to the reactantNums variable
                    if x == y:
                        reactantNums.append(j + 1)
                        print 'found: ', y, 'j = ', j
                        break
                    j += 1
                # This code only executes if the break is NOT called, i.e. if the loop exits cleanly without the
                # reactant being found in the known species
                else:
                    # Add reactant to speciesList, and increase the speciesListCounter. Finally, add this number to
                    # reactantNums to record this reaction.
                    speciesList.append(x)
                    speciesListCounter += 1
                    reactantNums.append(speciesListCounter)
                    print 'adding ', x, ' to speciesList'

            # print 'SpeciesList = ',speciesList
            # print 'reactantNums = ', reactantNums

            # Write the reactants to mechanism.reactemp
            for z in reactantNums:
                temp = str(reactionNumber) + ' ' + str(z)
                reac.write(temp)
                reac.write('\n')

        if not productsList.isspace():
            # Compare each product against known species.
            productNums = []
            for x in products:
                j = 0
                speciesNumFound = False
                for y in speciesList:
                    # Check for equality: if equality, then the product is a known species and its number should be
                    # added to the productNums variable
                    if x == y:
                        productNums.append(j + 1)
                        print 'found: ', y, 'j = ', j
                        break
                    j += 1
                # This code only executes if the break is NOT called, i.e. if the loop exits cleanly without the
                # product being found in the known species
                else:
                    # Add product to speciesList, and increase the speciesListCounter. Finally, add this number to
                    # productNums to record this reaction.
                    speciesList.append(x)
                    speciesListCounter += 1
                    productNums.append(speciesListCounter)
                    print 'adding ', x, ' to speciesList'

            # print 'SpeciesList = ',speciesList
            # print 'productNums = ', reactantNums

            # Write the products to mechanism.prod
            for z in productNums:
                temp = str(reactionNumber) + ' ' + str(z)
                prod.write(temp)
                prod.write('\n')

# MARK END OF FILE WITH ZEROS
reac.write('0\t0\t0\t0 \n')
prod.write('0\t0\t0\t0')
# Output number of species and number of reactions
st = str(speciesListCounter) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions\n'
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
# reac.seek(0)
# l = reac.readlines()
# # # print l
# st = str(len(speciesList))+' '+str(reactionNumber) + ' '
# # # print st
# # # reac.seek(0)
# # # reac.write(st)
# reac1 = open('./mech.reac1','w')
# reac1.write(st)
# for lines in l:
#	 reac1.write(lines)


# Write speciesList to mechanism.species, indexed by (1 to speciesListCounter)
for i, x in zip(range(1, speciesListCounter+1), speciesList):
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

# DO RO2 SUM
roFac = open('./RO2.fac')

ro2 = roFac.readlines()
counter = 0
ro2List = []
for l in ro2:
    counter += 1
    if counter == 1:
        strArray = l.split('=')
        l = strArray[1]

    strArray = l.split('+')

    print strArray
    for x in strArray[:]:
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

for ro2i in ro2List:
    print 'looping over inputted ro2s'
    print ro2List
    inFullList = False
    for ro2j in fRO2l:
        if ro2i.strip() == ro2j.strip():
            inFullList = True

    if inFullList:
        print ro2i.strip() + ' found in RO2List'
    else:
        print ro2i.strip() + ' not found in RO2List'
        s = '! ' + ro2i.strip() + ' is not in the MCM list of RO2 species. Should it be in the RO2 sum?\n'
        fortranFile.write(s)

# loop over RO2 to get species numbers and write
counter = 0
fortranFile.write('  ro2 = 0.00e+00\n')
for ro2i in ro2List:
    print 'ro2i: ' + ro2i
    counter = 0
    for y in speciesList:
        if ro2i.strip() == y.strip():
            speciesNumber = counter + 1
            st = '  ro2 = ro2 + y(' + str(speciesNumber) + ')!' + ro2i.strip() + '\n'
            fortranFile.write(st)
            # Exit loop early if species found
            break
        counter += 1
    # This code only executes if the break is NOT called, i.e. if the loop exits cleanly
    else:
        fortranFile.write('\t ! error RO2 not in mechanism: ')
        fortranFile.write(ro2i)
        fortranFile.write('\n')

fortranFile.write('\n\n')
# # DO NOY SUM
# NOYFac = open('./NOY.fac')
#
#
# NOY = NOYFac.readlines()
# counter = 0
# NOYList = []
# for n in NOY:
#	counter = counter + 1
#	if counter == 1:
#		strArray = n.split('=')
#		n = strArray[1]
#
#	strArray = n.split('+')
#
#	print strArray
#	for x in strArray[:]:
#		x = x.strip()
#		if x == '':
#			print 'doing nothing'
#		else:
#			print x
#			NOYList.append(x)
#
# # loop over NOY to get species numbers and write
# counter = 0
# speciesFound = 0
# fortranFile.write('\tNOY = 0.00e+00\n')
# for NOYi in NOYList:
#	speciesFound = 0
#	print 'NOYi: ' + NOYi
#	counter = 0
#	for y in speciesList:
#		if NOYi.strip() == y.strip():
#			speciesNumber = counter + 1
#			speciesFound = 1
#		counter = counter + 1
#
#
#	if (speciesFound == 1):
#		st = '\tNOY = NOY + y(' + str(speciesNumber) + ')!' + NOYi.strip() + '\n'
#		fortranFile.write(st)
#	elif (speciesFound == 0):
#		fortranFile.write('\t !error NOY not in mechanism: ')
#		fortranFile.write(NOYi)
#		fortranFile.write('\n')
#
# fortranFile.write('\n\n')
# # Combine mechanism rates and RO2 / NOY sum files
rates = open('./mechanism-rate-coefficients.ftemp')
rs = rates.readlines()

for r in rs:
    fortranFile.write(r)

os.remove('./mechanism.reactemp')
os.remove('./mechanism-rate-coefficients.ftemp')
