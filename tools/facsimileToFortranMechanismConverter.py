import re
import os

f = open('./mechanism.fac')
reac = open('./mechanism.reactemp', 'w')
prod = open('./mechanism.prod', 'w')
species = open('./mechanism.species', 'w')
mechRates = open('./mechanism-rate-coefficients.ftemp', 'w')

reac.write('numberOfSpecies numberOfReactions \n')

s = f.readlines()
print s

speciesListCounter = 0
speciesList = []
rateConstants = []
reactionNumber = 0
reacFileCounter = 0


for line in s:

    # CHECK FOR COMMENTS (ANY LINE STARTING WITH !)
    if re.match('!', line) is not None:
        rateConstants.append(line)
    # CHECK FOR BLANK LINES
    elif line.isspace():
        rateConstants.append(line)
    elif (re.match(';', line) is not None) | (re.match('[*]', line) is not None):
        st = '!'+line
        rateConstants.append(st)
    # OTHERWISE ASSUME ALL REACTIONS ARE IN THE CORRECT FORMAT SO PROCESS
    else:
        reactionNumber += 1
        print 'line =', line
        line = line.strip()
        line = line.strip('[;]')
        line = line.strip('[%]')
        print 'line =', line
        a = re.split(':', line)
        print 'a = ', a
        
        rateConstant = a[0]
        rateConstants.append(rateConstant)
        print rateConstant
        
        reaction = a[1]
        
        r = reaction.split(' ')
        print 'reaction = ', reaction
        
        string = re.split('=', reaction)
        print string
        
        reactantsList = string[0]
        productsList = string[1]
        
        print 'reactantlist = ', reactantsList
        print 'productlist = ', productsList
        
        reactants = re.split('[+]', reactantsList)
        products = re.split('[+]', productsList)
        
        print 'reactants = ', reactants
        print 'products = ', products
        
        if not reactantsList.isspace():
            # SEARCH FOR EXISTING REACTANTS
            reactantNums = []
            for x in reactants[:]:
                j = 0
                speciesNumFound = False
                # reactantNums = []
                for y in speciesList:
                    # print 'j = ', j
                    # print 'speciesList = ', speciesList
                    if y == x.strip():
                        reactantNums.append(j+1)
                        speciesNumFound = True
                        print 'found: ', y, 'j = ', j
                    j += 1
                
                if not speciesNumFound:
                    speciesList.append(x.strip())
                    speciesListCounter += 1
                    reactantNums.append(speciesListCounter)
                    print 'adding ', x.strip(), ' to speciesList' 

            # print 'SpeciesList = ',speciesList
            # print 'reactantNums = ', reactantNums
        
            # WRITE THE REACTANTS TO mechanism.reactemp FILE
            for z in reactantNums:
                temp = str(reactionNumber) + ' ' + str(z)
                reac.write(temp)
                reac.write('\n')
        
        if not productsList.isspace():
            # SEARCH FOR EXISTING REACTANTS
            i = 0
            productNums = []
            for x in products[:]:
                j = 0
                speciesNumFound = False
                # productNums = []
                for y in speciesList:
                    # print 'j = ', j
                    # print 'speciesList = ', speciesList
                    if y == x.strip():
                        productNums.append(j+1)
                        speciesNumFound = True
                        print 'found: ', y, 'j = ', j
                    j += 1

                if not speciesNumFound:
                    speciesList.append(x.strip())
                    speciesListCounter += 1
                    productNums.append(speciesListCounter)
                    print 'adding ', x.strip(), ' to speciesList' 
                i += 1
            # print 'SpeciesList = ',speciesList
            # print 'productNums = ', reactantNums
        
            # WRITE THE PRODUCTS TO mechanism.prod FILE
            for z in productNums:
                temp = str(reactionNumber) + ' ' + str(z)
                prod.write(temp)
                prod.write('\n')

# MARK END OF FILE WITH ZEROS
reac.write('0\t0\t0\t0 \n')
prod.write('0\t0\t0\t0')
size = len(speciesList)
st = str(size) + ' ' + str(reactionNumber) + ' numberOfSpecies numberOfReactions'
reac.write(st)
reac.close()

# REARRANGE MECHANISM.REAC FORMAT TO MAKE IT READABLE BY THE MODEL
reac1 = open('./mechanism.reactemp')
reacFin = open('./mechanism.reac', 'w')
st = reac1.readlines()
reacFin.write(st[len(st)-1])

counter = 0
for line in st:
    counter += 1
    if counter < len(st):
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
#     reac1.write(lines)


# WRITE OUT TO mechanism.species
i = 1
for x in speciesList:
    st = str(i) + ' ' + str(x)
    species.write(st)
    species.write('\n')
    i += 1
    
# WRITE OUT RATE COEFFICIENTS
i = 1
counter = 0
for x in rateConstants:
    if re.match('!', x) is not None:
        mechRates.write(str(x))
        counter += 1
    elif x.isspace():
        mechRates.write(str(x))
        counter += 1
    else:
        string = x.replace('@', '**')
        string = string.replace('<', '(')
        string = string.replace('>', ')')
        st = '  p(' + str(i) + ') = ' + string + '  !' + s[counter]
        mechRates.write(st)
        mechRates.write('\n')
        i += 1
        counter += 1
    
# for x in reactants[:]:
#     x = x.strip()
#     print x
# for x in products[:]:
#     x = x.strip()
#     print x

#     print reactants
#     print products
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
    inFullList = 0
    for ro2j in fRO2l:
        if ro2i.strip() == ro2j.strip():
            inFullList = 1
            
    if inFullList == 1:
        print ro2i.strip() + ' found in RO2List'
    elif inFullList == 0:
        print ro2i.strip() + ' not found in RO2List'
        s = '! ' + ro2i.strip() + ' is not in the MCM list of RO2 species. Should it be in the RO2 sum?\n'
        fortranFile.write(s)

# loop over RO2 to get species numbers and write
counter = 0
speciesFound = 0
# fortranFile.write(str(ro2List))
fortranFile.write('  ro2 = 0.00e+00\n')
for ro2i in ro2List:
    speciesFound = 0
    print 'ro2i: ' + ro2i
    counter = 0
    for y in speciesList:
        if ro2i.strip() == y.strip():
            speciesNumber = counter + 1
            speciesFound = 1
        counter += 1

    if speciesFound == 1:
        st = '  ro2 = ro2 + y(' + str(speciesNumber) + ')!' + ro2i.strip() + '\n'
        fortranFile.write(st)
    elif speciesFound == 0:
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
#    counter = counter + 1
#    if counter == 1:
#        strArray = n.split('=')
#        n = strArray[1]
#
#    strArray = n.split('+')
#
#    print strArray
#    for x in strArray[:]:
#        x = x.strip()
#        if x == '':
#            print 'doing nothing'
#        else:
#            print x
#            NOYList.append(x)
#
# # loop over NOY to get species numbers and write
# counter = 0
# speciesFound = 0
# fortranFile.write('\tNOY = 0.00e+00\n')
# for NOYi in NOYList:
#    speciesFound = 0
#    print 'NOYi: ' + NOYi
#    counter = 0
#    for y in speciesList:
#        if NOYi.strip() == y.strip():
#            speciesNumber = counter + 1
#            speciesFound = 1
#        counter = counter + 1
#
#
#    if (speciesFound == 1):
#        st = '\tNOY = NOY + y(' + str(speciesNumber) + ')!' + NOYi.strip() + '\n'
#        fortranFile.write(st)
#    elif (speciesFound == 0):
#        fortranFile.write('\t !error NOY not in mechanism: ')
#        fortranFile.write(NOYi)
#        fortranFile.write('\n')
#
# fortranFile.write('\n\n')
# # Combine mechanism rates and RO2 / NOY sum files
rates = open('./mechanism-rate-coefficients.ftemp')
rs = rates.readlines()

for r in rs:
    fortranFile.write(r)
        
os.remove('./mechanism.reactemp')
os.remove('./mechanism-rate-coefficients.ftemp')
