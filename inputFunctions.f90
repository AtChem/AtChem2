SUBROUTINE readJFacSpecies ()
  USE photolysisRates
  USE directories, ONLY: param_dir
  IMPLICIT NONE
  INTEGER :: i

  jfacSpeciesLine = 0
  WRITE (*,*) 'Reading JFacSpecies...'
  OPEN (10, file=trim(param_dir) // '/JFacSpecies.config', status='old')
  READ (10,*) jfacBase
  CLOSE (10, status='keep')
  WRITE (*,*) 'JFacSpecies = ', jfacBase
  WRITE (*,*) 'Finished reading JFacSpecies.'
  ! get line number for the JFac base species:
  DO i = 1, nrOfPhotoRates
     IF ((TRIM (photoRateNames(i)))==(TRIM (jfacBase))) THEN
        jfacSpeciesLine = i
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE readJFacSpecies

SUBROUTINE readPhotoloysisRates (ck, cl, cmm, cnn, str, tf)
  USE photolysisRates, ONLY: useConstantValues, maxNrOfPhotoRates, nrOfPhotoRates
  USE directories, ONLY: param_dir
  IMPLICIT NONE
  INTEGER :: i, ck(*), ierr
  DOUBLE PRECISION :: cl(*), cmm(*), cnn(*), tf(*)
  CHARACTER (LEN=30) :: str(*)

  WRITE (*,*) 'Reading photolysis rates from file...'
  OPEN (10, file=trim(param_dir) // '/photolysisRates.config', status='old')
  READ (10,*)
  DO i = 1, maxNrOfPhotoRates
     READ (10,*, iostat=ierr) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
     IF (ierr/=0) THEN
        EXIT
     ENDIF
  ENDDO
  CLOSE (10, status='keep')
  nrOfPhotoRates = i-1
  i = 1
  WRITE (*,*) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
  i = nrOfPhotoRates
  WRITE (*,*) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
  WRITE (*,*) 'Finished reading photolysis rates.'
  WRITE (*,*) 'Number of photolysis rates:', nrOfPhotoRates
  RETURN
END SUBROUTINE readPhotoloysisRates

SUBROUTINE readPhotoloysisConstants (ck, cl, cmm, cnn, str, tf)
  USE photolysisRates, ONLY: useConstantValues, maxNrOfPhotoRates, nrOfPhotoRates
  USE directories, ONLY: param_dir
  IMPLICIT NONE
  INTEGER :: i, ck(*), ierr
  DOUBLE PRECISION :: cl(*), cmm(*), cnn(*), tf(*)
  CHARACTER (LEN=30) :: str(*)
  LOGICAL :: file_exists

! Check whether file exists correctly in readPhotoloysisConstants,
  WRITE (*,*) 'Looking for photolysis constants file...'
  INQUIRE(FILE=trim(param_dir) // '/photolysisConstants.config', EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
     useConstantValues = 0
     WRITE (*,*) 'Photolysis constants file not found, trying photolysis rates file...'
     CALL readPhotoloysisRates (ck, cl, cmm, cnn, str, tf)
     RETURN
  ENDIF
  useConstantValues = 1
  WRITE (*,*) 'Reading photolysis constants from file...'
  OPEN (10, file=trim(param_dir) // '/photolysisConstants.config', status='old', iostat=ierr)
  READ (10,*)
  DO i = 1, maxNrOfPhotoRates
     READ (10,*, iostat=ierr) ck(i), cl(i), str(i)
     IF (ierr/=0) THEN
        EXIT
     ENDIF
  ENDDO
  CLOSE (10, status='keep')
  nrOfPhotoRates = i-1
  i = 1
  WRITE (*,*) ck(i), cl(i), str(i)
  i = nrOfPhotoRates
  WRITE (*,*) ck(i), cl(i), str(i)
  WRITE (*,*) 'Finished reading photolysis constants.'
  WRITE (*,*) 'Number of photolysis rates:', nrOfPhotoRates
  RETURN
END SUBROUTINE readPhotoloysisConstants


SUBROUTINE getReactionListSizes (csize1, csize2)
  INTEGER :: csize1, csize2, k, l

  OPEN (10, file='modelConfiguration/mechanism.reac', status='old') ! input file for lhs of equations
  csize1 = 0
  READ (10,*)
  DO
     READ (10,*) k, l
     IF (k==0) EXIT
     csize1 = csize1+1
  ENDDO
  CLOSE (10, status='keep')

  OPEN (11, file='modelConfiguration/mechanism.prod', status='old') ! input file for rhs of equations
  csize2 = 0
  DO
     READ (11,*) k, l
     IF (k==0) EXIT
     csize2 = csize2+1
  ENDDO
  CLOSE (11, status='keep')

  RETURN
END SUBROUTINE getReactionListSizes

SUBROUTINE getParametersFromFile (str, parameterArray)
  CHARACTER :: str*(*)
  DOUBLE PRECISION :: parameterArray(*)
  INTEGER :: i

  ! READ IN SOLVER PARAMETERS
  OPEN (10, file=str, status='old') ! input file
  i = 1
  READ (10,*) parameterArray(i)
  DO WHILE (parameterArray(i)/=-9999)
     i = i + 1
     READ (10,*) parameterArray(i)
  END DO
  CLOSE (10, status='keep')

  RETURN
END SUBROUTINE getParametersFromFile

SUBROUTINE readPhotoRates (maxNumberOfDataPoints)

  USE photolysisRates
  USE directories, ONLY: param_dir
  IMPLICIT NONE

  INTEGER :: counter, i, k
  INTEGER :: maxNumberOfDataPoints
  CHARACTER (LEN=30) :: string
  CHARACTER (LEN=27) :: fileLocationPrefix
  CHARACTER (LEN=57) :: fileLocation

  ! GET NAMES OF PHOTO RATES
  CALL readPhotoloysisConstants (ck, cl, cmm, cnn, photoRateNames, transmissionFactor)
  WRITE (*,*)
  ! GET NAMES OF CONSTRAINED PHOTO RATES
  WRITE (*,*) 'Reading names of constrained photolysis rates from file...'

  OPEN (10, file=trim(param_dir) // '/constrainedPhotoRates.config', status='old') ! input file
  counter = 0
  DO
     counter = counter + 1
     READ (10,*) constrainedPhotoRates(counter)
     IF (constrainedPhotoRates(counter)=='end') EXIT
  ENDDO
  CLOSE (10, status='keep')
  numConPhotoRates = counter -1
  WRITE (*,*) 'Finished reading names of constrained photolysis rates.'
  WRITE (*,*) 'Number of constrained photorates:', numConPhotoRates
  IF (numConPhotoRates>0) WRITE (*,*) 1, constrainedPhotoRates(1)
  IF (numConPhotoRates>2) WRITE (*,*) '...'
  IF (numConPhotoRates>1) WRITE (*,*) numConPhotoRates, constrainedPhotoRates(numConPhotoRates)

  ! GET NUMBERS OF CONSTRAINED PHOTO RATES
  DO i = 1, numConPhotoRates
     DO k = 1, nrOfPhotoRates
        IF (constrainedPhotoRates(i)==photoRateNames(k)) THEN
           constrainedPhotoRatesNumbers(i) = ck(k)
        ENDIF
     ENDDO
  ENDDO
  ! ALLOCATE ARRAY SIZE FOR STOREAGE OF PHOTOLYSIS CONSTRAINT DATA
  ALLOCATE (photoX (numConPhotoRates, maxNumberOfDataPoints))
  ALLOCATE (photoY (numConPhotoRates, maxNumberOfDataPoints))
  ALLOCATE (photoY2 (numConPhotoRates, maxNumberOfDataPoints))
  ALLOCATE (photoNumberOfPoints(numConPhotoRates))

  fileLocationPrefix = './environmentalConstraints/'

  ! READ IN PHOTOLYSIS DATA
  IF (numConPhotoRates>0) THEN
     WRITE (*,*) 'Reading in constraint data for photolysis rates...'
     DO i = 1, numConPhotoRates
        string = constrainedPhotoRates(i)
        WRITE (*,*) string, '...'
        fileLocation = fileLocationPrefix // string
        OPEN (11, file=fileLocation, status='old')
        READ (11,*) photoNumberOfPoints(i)
        DO k = 1, photoNumberOfPoints(i)
           READ (11,*) photoX (i, k), photoY (i, k) !, photoY2 (i, k)
        ENDDO
        CLOSE (11, status='keep')
     ENDDO
     WRITE (*,*) 'Finished reading constraint data for photolysis rates.'
  ENDIF
  RETURN
END SUBROUTINE readPhotoRates

SUBROUTINE readSpeciesOutputRequired (r, i, nsp)
  USE directories, ONLY: param_dir

  CHARACTER (LEN=10) c, r(*)
  INTEGER i, nsp

  WRITE (*,*) 'Reading concentration output from file...'

  i = 1
  c = 'abc'

  OPEN (10, file=trim(param_dir) // '/concentrationOutput.config', status='old')
  ! Loop over all lines of the file, and add each entry to r(i)
  ! Then check we don't have more species of interest than total species
  READ (10,*) c
  DO WHILE (c/='end' .AND. i<=nsp )
     r(i) = c
     i = i + 1
     READ (10,*) c
  ENDDO
  CLOSE (10, status='keep')

  WRITE (*,*) 'Finished reading concentration output from file.'
  i = i - 1

  ! ERROR HANDLING
  IF (i>nsp) THEN
     WRITE (51,*) 'Error: Number of (number of species output is required for) > (number of species) '
     WRITE (51,*) "(number of species output is required for) = ", i
     WRITE (51,*) "(number of species) = ", nsp
     STOP 2
  ENDIF

  RETURN
END SUBROUTINE readSpeciesOutputRequired

SUBROUTINE readSpecies (y, neq, speciesName, speciesNumber)
  USE directories, ONLY: param_dir

  DOUBLE PRECISION y(*)
  INTEGER neq, speciesNumber(*), j
  CHARACTER (LEN=10) speciesName(*)

  ! READ IN INITIAL CONCENTRATIONS
  OPEN (10, file='modelConfiguration/mechanism.species') ! input file
  DO j = 1, neq
     READ (10,*) speciesNumber(j), speciesName(j)
     y(j) = 0
  ENDDO
  CLOSE (10, status='keep')

  RETURN
END SUBROUTINE readSpecies

SUBROUTINE readConcentrations (concSpeciesName, concentration, concCounter, nsp)
  USE directories, ONLY: param_dir

  CHARACTER (LEN=10) concSpeciesName(*), k
  DOUBLE PRECISION concentration(*), l
  INTEGER i, concCounter, nsp

  WRITE (*,*) 'Reading initial concentrations...'

  i = 1
  OPEN (10, file=trim(param_dir) // '/initialConcentrations.config', status='old') ! input file for lhs of equations
  DO
     READ (10,*) k, l
     IF (l==-1) EXIT
     concentration(i) = l
     concspeciesName(i) = k

     i = i + 1
  ENDDO
  CLOSE (10, status='keep')

  WRITE (*,*) 1, ' ', concspeciesName(1), ' ', concentration(1)
  WRITE (*,*) '...'
  WRITE (*,*) i-1, ' ', concspeciesName(i-1), ' ', concentration(i-1)

  WRITE (*,*) 'Finished reading initial concentrations.'

  concCounter = i - 1

  IF (concCounter>nsp) THEN
     WRITE (51,*) "Error:(number of species initial concentrations are set for) > (number of species) "
     WRITE (51,*) "(number of species initial concentrations are set for) = ", concCounter
     WRITE (51,*) "(number of species) = ", nsp
  ENDIF

  RETURN
END SUBROUTINE readConcentrations

SUBROUTINE readProductsOfInterest (r, i)
  USE directories, ONLY: param_dir

  CHARACTER (LEN=10) c, r(*)
  INTEGER i

  WRITE (*,*) 'Reading products of interest...'

  OPEN (10, file=trim(param_dir) // '/productionRatesOutput.config', status='old')
  i = 0
  c = 'abc'
  READ (10,*) c
  DO WHILE (c/='end')
     i = i + 1
     r(i) = c
     READ (10,*) c
  ENDDO
  CLOSE (10, status='keep')
  IF (i>0) THEN
     WRITE (*,*) 1, r(1)
  ENDIF
  IF (i>2) THEN
     WRITE (*,*) '...'
  ENDIF
  IF (i>1) THEN
     WRITE (*,*) i, r(i)
  ENDIF
  WRITE (*,*) 'Finished reading products of interest.'
  RETURN
END SUBROUTINE readProductsOfInterest

SUBROUTINE readReactantsOfInterest (r, i)
  USE directories, ONLY: param_dir

  CHARACTER (LEN=10) c, r(*)
  INTEGER i
  WRITE (*,*) 'Reading reactants of interest...'

  i = 0
  c = 'abc'
  OPEN (10, file=trim(param_dir) // '/lossRatesOutput.config', status='old')
  READ (10,*) c
  DO WHILE (c/='end')
     i = i + 1
     r(i) = c
     READ (10,*) c
  ENDDO
  CLOSE (10, status='keep')
  IF (i>0) THEN
     WRITE (*,*) 1, r(1)
  ENDIF
  IF (i>2) THEN
     WRITE (*,*) '...'
  ENDIF
  IF (i>1) THEN
     WRITE (*,*) i, r(i)
  ENDIF
  WRITE (*,*) 'Finished reading reactants of interest.'
  RETURN

END SUBROUTINE readReactantsOfInterest

SUBROUTINE readSpeciesConstraints (speciesName, neq, y, t)
  USE species
  USE constraints
  USE chemicalConstraints
  USE directories, ONLY: param_dir

  IMPLICIT NONE

  INTEGER :: i, j, k, dataNumberOfPoints, neq, id
  INTEGER :: countOfVarConSpecNames, countOfFixConSpecNames, countOfConNames
  CHARACTER (LEN=13) :: string
  CHARACTER (LEN=10) :: speciesName(*), name
  CHARACTER (LEN=21) :: fileLocationPrefix
  CHARACTER (LEN=57) :: fileLocation
  DOUBLE PRECISION :: concAtT, t, value
  DOUBLE PRECISION :: y (*)

  ! READ IN SPECIES TO BE CONSTRAINED
  WRITE (*,*) 'Counting the species to be constrained (in file constrainedSpecies.config)...'
  OPEN (10, file=trim(param_dir) // '/constrainedSpecies.config', status='old') ! input file

  i = 0
  READ (10,*) string
  DO WHILE (string/='end')
     i = i + 1
     READ (10,*) string
  END DO
  CLOSE (10, status='keep')
  countOfVarConSpecNames = i

  WRITE (*,*) 'Finished counting the names of the species to be constrained.'
  WRITE (*,*) 'Number of names for variable constrained species:', countOfVarConSpecNames

  ! read in numberOfFixedConstrainedSpecies

  WRITE (*,*) 'Counting the fixed-concentration species to be constrained (in file constrainedFixedSpecies.config)...'
  OPEN (11, file=trim(param_dir) // '/constrainedFixedSpecies.config', status='old') ! input file
  i = 0
  READ (11,*) string
  DO WHILE (string/='end')
     i = i + 1
     READ (11,*) string
  END DO
  CLOSE (11, status='keep')
  WRITE (*,*) 'Finished counting the names of fixed-concentration species'
  countOfFixConSpecNames = i
  WRITE (*,*) 'Number names of fixed constrained species:', countOfFixConSpecNames

  countOfConNames = countOfVarConSpecNames + countOfFixConSpecNames
  ALLOCATE (constrainedSpecies(countOfConNames), constrainedName(countOfConNames))



  WRITE (*,*) 'Reading in the names of variable constrained species...'
  OPEN (12, file=trim(param_dir) // '/constrainedSpecies.config', status='old') ! input file
  i = 0
  READ (12,*) name
  DO WHILE (name/='end')
     CALL matchOneNameToNumber (speciesName, name, neq, id)
     IF (id/=0) THEN
        i = i + 1
        constrainedName(i) = name
        constrainedSpecies(i) = id
     ENDIF
     READ (12,*) name
  END DO
  CLOSE (12, status='keep')
  numberOfVariableConstrainedSpecies = i
  WRITE (*,*) 'Finished reading the names of variable constrained species'
  WRITE (*,*) 'Number of constrained variable species:', numberOfVariableConstrainedSpecies

  WRITE (*,*) 'maxNumberOfDataPoints:', maxNumberOfDataPoints
  WRITE (*,*) 'Allocating storage for variable constrained species...'
  ALLOCATE (dataX (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
  ALLOCATE (dataY (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
  ALLOCATE (dataY2 (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
  WRITE (*,*) 'Finished allocating storage for variable constrained species.'


  IF (numberOfVariableConstrainedSpecies>0) WRITE (*,*) 1, constrainedName(1)
  IF (numberOfVariableConstrainedSpecies>2) WRITE (*,*) '...'
  IF (numberOfVariableConstrainedSpecies>1) WRITE (*,*) numberOfVariableConstrainedSpecies, &
       constrainedName(numberOfVariableConstrainedSpecies)

  fileLocationPrefix = './speciesConstraints/'

  ! READ CONCENTRATION DATA FOR VARIABLE CONSTRAINED SPECIES
  WRITE (*,*) 'Reading concentration data for constrained species...'
  ALLOCATE (speciesNumberOfPoints(numberOfVariableConstrainedSpecies+countOfFixConSpecNames))
  DO i = 1, numberOfVariableConstrainedSpecies
     IF (i<3 .OR. i==numberOfVariableConstrainedSpecies) THEN
        WRITE (*,*) constrainedName(i), '...'
     ELSE
        IF (i==2) WRITE (*,*) '...'
     ENDIF

     fileLocation = fileLocationPrefix // TRIM (constrainedName(i))
     OPEN (13, file=fileLocation, status='old')

     READ (13,*) dataNumberOfPoints
     IF (dataNumberOfPoints>maxNumberOfDataPoints) THEN
        dataNumberOfPoints = maxNumberOfDataPoints
        WRITE (*,*) 'Warning! Truncated constraint data to', dataNumberOfPoints, '.'
     ENDIF

     speciesNumberOfPoints(i) = dataNumberOfPoints
     DO k = 1, dataNumberOfPoints
        READ (13,*) dataX (i, k), dataY (i, k) !, dataY2 (i, k)
     ENDDO
     CLOSE (13, status='keep')

  ENDDO


  ! READ IN NAMES AND CONCENTRATION DATA FOR FIXED CONSTRAINED SPECIES
  ALLOCATE (dataFixedY (countOfFixConSpecNames))
  WRITE (*,*) 'Reading in the names and concentration of the fixed constrained species (in file constrainedFixedSpecies.config)...'
  OPEN (14, file=trim(param_dir) // '/constrainedFixedSpecies.config', status='old') ! input file
  id = 0
  j = 0
  DO i = 1, countOfFixConSpecNames
     READ (14,*) name, value
     CALL matchOneNameToNumber (speciesName, name, neq, id)
     IF (id/=0) THEN
        j = j+1
        constrainedName(j+numberOfVariableConstrainedSpecies) = name
        dataFixedY (j) = value
        constrainedSpecies(j+numberOfVariableConstrainedSpecies) = id
     ENDIF
  END DO
  CLOSE (14, status='keep')
  numberOfFixedConstrainedSpecies = j
  WRITE (51,*) 'Number of fixed constrained species:', numberOfFixedConstrainedSpecies

  IF (numberOfFixedConstrainedSpecies>0) THEN
     WRITE (*,*) 1, constrainedName(1+numberOfVariableConstrainedSpecies), dataFixedY (1)
  ENDIF
  IF (numberOfFixedConstrainedSpecies>2) WRITE (*,*) '...'
  IF (numberOfFixedConstrainedSpecies>1) THEN
     WRITE (*,*) numberOfFixedConstrainedSpecies, &
          constrainedName(numberOfFixedConstrainedSpecies+numberOfVariableConstrainedSpecies), &
          dataFixedY (numberOfFixedConstrainedSpecies)
  ENDIF
  WRITE (*,*) 'Finished reading in the names and concentration of fixed-concentration species.'

  numberOfConstrainedSpecies = numberOfVariableConstrainedSpecies + numberOfFixedConstrainedSpecies
  WRITE (51,*) "Number of constrained species:", numberOfConstrainedSpecies

  ! ERROR HANDLING
  IF (numberOfConstrainedSpecies>=neq) THEN
     WRITE (51,*) "Error: Number of (number of constrained species) >= (number of species) "
     WRITE (51,*) "(number of constrained species) = ", numberOfConstrainedSpecies
     WRITE (51,*) "(number of species) = ", neq
     STOP 2
  ENDIF

  ALLOCATE (constrainedConcs(numberOfConstrainedSpecies))

  CALL setNumberOfConstrainedSpecies (numberOfConstrainedSpecies)

  WRITE (*,*) 'Finished reading constrained species.'

  ! initialise concentrations of constrained species
  WRITE (*,*) 'Initialising concentrations of constrained species...'
  DO i = 1, numberOfConstrainedSpecies
     IF (i<=numberOfVariableConstrainedSpecies) THEN
        CALL getConstrainedQuantAtT2D (t, datax, datay, datay2, speciesNumberOfPoints(i), concAtT, 1, i, &
             maxNumberOfDataPoints, numberOfVariableConstrainedSpecies)
     ELSE
        concAtT = dataFixedY (i-numberOfVariableConstrainedSpecies)
     ENDIF
     constrainedConcs(i) = concAtT
     CALL setConstrainedConc (i, concAtT)
     y(constrainedSpecies(i)) = concAtT
  ENDDO
  WRITE (*,*) 'Finished initialising concentrations of constrained species.'

  RETURN
END SUBROUTINE readSpeciesConstraints

SUBROUTINE readEnvVar (maxNumberOfDataPoints)
  USE envVars
  USE directories, ONLY: param_dir

  IMPLICIT NONE

  INTEGER :: i, counter, numConEnvVar, k, maxNumberOfDataPoints
  CHARACTER (LEN=30) dummy
  CHARACTER (LEN=27) :: fileLocationPrefix
  CHARACTER (LEN=57) :: fileLocation
  DOUBLE PRECISION, ALLOCATABLE :: testArray(:)

  WRITE (*,*) 'Reading environment variables...'
  OPEN (10, file=trim(param_dir) // '/environmentVariables.config', status='old') ! input file
  maxNumberOfDataPoints = 10000

  ! FIND NUMBER OF ENVIRONMENTAL VARIABLES
  counter = 0
  DO
     counter = counter + 1
     READ (10,*) dummy
     IF (dummy=='end') EXIT
  ENDDO

  numEnvVars = counter -1

  !ALLOCATE STORAGE FOR CURRENT VALUES OF ENV VARS USED FOR OUTPUT
  ALLOCATE (currentEnvVarValues(numEnvVars))

  WRITE (*,*) 'Number of environment variables: ', numEnvVars
  ALLOCATE (testArray(3))
  ALLOCATE (envVarTypesNum(numEnvVars), envVarNames(numEnvVars), envVarTypes(numEnvVars))
  ALLOCATE (envVarFixedValues(numEnvVars))
  REWIND (10)
  ! READ IN ENV VARIABLES
  numConEnvVar = 0
  DO i = 1, numEnvVars
     READ (10,*) dummy, envVarNames(i), envVarTypes(i)
     WRITE (*,*) dummy, envVarNames(i), envVarTypes(i)

     IF (TRIM (envVarTypes(i))=='CALC') THEN
        envVarTypesNum(i) = 1
     ELSE IF (TRIM (envVarTypes(i))=='CONSTRAINED') THEN
        envVarTypesNum(i) = 2
        numConEnvVar = numConEnvVar + 1
     ELSE IF (TRIM (envVarTypes(i))=='NOTUSED') THEN
        envVarTypesNum(i) = 4
        ! OTHERWISE ASSUME A FIXED VALUE
     ELSE
        envVarTypesNum(i) = 3
        READ (envVarTypes(i),*) envVarFixedValues(i)
     ENDIF
  ENDDO
  CLOSE (10, status='keep')

  WRITE (*,*) 'Finished reading environment variables.'
  WRITE (*,*)

  ALLOCATE (envVarX (numEnvVars, maxNumberOfDataPoints))
  ALLOCATE (envVarY (numEnvVars, maxNumberOfDataPoints))
  ALLOCATE (envVarY2 (numEnvVars, maxNumberOfDataPoints))
  ALLOCATE (envVarNumberOfPoints(numEnvVars))

  fileLocationPrefix = './environmentalConstraints/'
  ! READ IN CONSTRAINT DATA FOR CONSTRAINED ENV VARIABLES
  WRITE (*,*) 'Checking for constrained environmental variables...'
  DO i = 1, numEnvVars
     IF (envVarTypes(i)=='CONSTRAINED') THEN

        WRITE (*,*) 'Reading constraint data for', envVarNames(i)

        fileLocation = fileLocationPrefix // TRIM (envVarNames(i))

        OPEN (11, file=fileLocation, status='old')

        READ (11,*) envVarNumberOfPoints(i)
        DO k = 1, envVarNumberOfPoints(i)
           READ (11,*) envVarX (i, k), envVarY (i, k) ! envVarY2 (i, k)
        ENDDO
        CLOSE (11, status='keep')
        WRITE (*,*) 'Finished reading constraint data.'
     ENDIF
  ENDDO
  ! deallocate data
  DEALLOCATE (testArray)
  WRITE (*,*) 'Finished checking for constrained environmental variables.'

  RETURN
END SUBROUTINE readEnvVar
