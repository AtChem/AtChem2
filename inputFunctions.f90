MODULE inputFunctions_mod
CONTAINS
SUBROUTINE readReactions (lhs, rhs, coeff)
  ! Reads in the data from mC/mechanism.reac and mC/mechanism.prod
  INTEGER :: k, l, size1, size2
  INTEGER, intent(out) :: lhs(:, :), rhs(:, :)
  DOUBLE PRECISION, intent(out) :: coeff(:)
  IF (size( lhs, 1 )/=3) THEN
    STOP "size( lhs, 1 )/=3 in readReactions()."
  END IF
  IF (size( rhs, 1 )/=2) THEN
    STOP "size( rhs, 1 )/=2 in readReactions()."
  END IF
  IF (size(coeff)/=size(rhs, 2)) THEN
    STOP "size(coeff)/=size(rhs, 2) in readReactions()."
  END IF
  WRITE (*,*) 'Reading reactants (lhs) from mechanism.rec...'
  OPEN (10, file='modelConfiguration/mechanism.reac', status='old') ! input file for lhs of equations

  ! read data for lhs of equations
  size1 = 0
  READ (10,*)
  DO
     READ (10,*) k, l

     IF (k==0) EXIT
     size1 = size1+1
     lhs(1, size1) = k
     lhs(2, size1) = l
     lhs(3, size1) = 1

  ENDDO

  WRITE (*,*) 'Reading products (rhs) from mechanism.prod...'
  OPEN (11, file='modelConfiguration/mechanism.prod', status='old') ! input file for rhs of equations
  ! read data for rhs of equations
  size2 = 0
  DO
     READ (11,*) k, l
     IF (k==0) EXIT
     size2 = size2+1
     rhs(1, size2) = k
     rhs(2, size2) = l
     coeff(size2) = 1

  ENDDO

  CLOSE (10, status='keep')
  CLOSE (11, status='keep')

  WRITE (*,*) 'Finished reading lhs and rhs data.'
  RETURN
END SUBROUTINE readReactions


SUBROUTINE readJFacSpecies ()
  ! Read modelConfiguration/JFacSpecies.config, and store this in jFacSpecies.
  ! Test this against known species, and if it is known then set jfacSpeciesLine
  ! to that line number in photoRateNames
  USE photolysisRates
  USE directories, ONLY: param_dir
  IMPLICIT NONE
  INTEGER :: i, ierr
  LOGICAL :: file_exists

  jfacSpeciesLine = 0
  WRITE (*,*) 'Reading JFacSpecies...'
  INQUIRE(file=trim(param_dir) // '/JFacSpecies.config', EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
     WRITE (*,*) "No JFacSpecies.config file exists, so setting jFacSpecies to ''"
     jFacSpecies = ''
  ELSE
     OPEN (10, file=trim(param_dir) // '/JFacSpecies.config', status='old', iostat=ierr)
     READ (10, *, iostat=ierr) jFacSpecies
     CLOSE (10, status='keep')
     ! Catch the case where the file is empty
     IF (ierr/=0) THEN
        jFacSpecies = ''
        RETURN
     END IF
  END IF
  ! Convert 'end' to '' so we only need to test for that later.
  IF ('end'==trim(jFacSpecies)) THEN
     jFacSpecies = ''
  END IF
  WRITE (*,*) 'JFacSpecies = ', trim(jFacSpecies)
  WRITE (*,*) 'Finished reading JFacSpecies.'
  ! get line number for the JFac base species:
  DO i = 1, nrOfPhotoRates
     IF ((trim(photoRateNames(i)))==(trim(jFacSpecies))) THEN
        jfacSpeciesLine = i
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE readJFacSpecies


SUBROUTINE readPhotolysisRates (ck, cl, cmm, cnn, str, tf)
  ! This is called from readPhotolysisConstants if modelConfiguration/photolysisConstants.config
  ! doesn't exist. It reads ck, cl, cmm, cnn, str, and tf from
  ! modelConfiguration/photolysisRates.config.
  USE photolysisRates, ONLY : maxNrOfPhotoRates, nrOfPhotoRates
  USE directories, ONLY : param_dir
  USE storage, ONLY : maxPhotoRateNameLength
  USE, INTRINSIC :: iso_fortran_env, ONLY : stderr=>error_unit
  IMPLICIT NONE

  INTEGER :: i, ck(*), ierr
  DOUBLE PRECISION :: cl(*), cmm(*), cnn(*), tf(*)
  CHARACTER (LEN=maxPhotoRateNameLength) :: str(*)

  WRITE (*,*) 'Reading photolysis rates from file...'
  OPEN (10, file=trim(param_dir) // '/photolysisRates.config', status='old')
  ! Ignore first line
  READ (10,*)
  DO i = 1, maxNrOfPhotoRates
     READ (10,*, iostat=ierr) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
     IF (ierr/=0) THEN
       ! We've reached the end of file, so exit this loop
        EXIT
     ENDIF
     nrOfPhotoRates = i
  ENDDO
  CLOSE (10, status='keep')

  IF (nrOfPhotoRates>3) THEN
     WRITE (*,*) ck(1), cl(1), cmm(1), cnn(1), str(1), tf(1)
     WRITE (*,*) '...'
     WRITE (*,*) ck(nrOfPhotoRates), cl(nrOfPhotoRates), cmm(nrOfPhotoRates), &
                 cnn(nrOfPhotoRates), str(nrOfPhotoRates), tf(nrOfPhotoRates)
  ELSE
     DO i = 1, nrOfPhotoRates
        WRITE (*,*) ck(i), cl(i), cmm(i), cnn(i), str(i), tf(i)
     ENDDO
  ENDIF
  WRITE (*,*) 'Finished reading photolysis rates.'
  WRITE (*,*) 'Number of photolysis rates:', nrOfPhotoRates
  RETURN
END SUBROUTINE readPhotolysisRates


SUBROUTINE readPhotolysisConstants (ck, cl, cmm, cnn, str, tf)
  ! If modelConfiguration/photolysisConstants.config exists, then read in
  ! 3 values to fill ck, cl and str.
  ! Otherwise, call ReadPhotolysisRates to fill ck, cl, cmm, cnn, str and tf.
  USE photolysisRates, ONLY: usePhotolysisConstants, maxNrOfPhotoRates, nrOfPhotoRates
  USE directories, ONLY: param_dir
  USE storage, ONLY : maxPhotoRateNameLength
  IMPLICIT NONE

  INTEGER :: i, ck(*), ierr
  DOUBLE PRECISION :: cl(*), cmm(*), cnn(*), tf(*)
  CHARACTER (LEN=maxPhotoRateNameLength) :: str(*)
  LOGICAL :: file_exists

! Check whether file exists correctly in readPhotolysisConstants,
  WRITE (*,*) 'Looking for photolysis constants file...'
  INQUIRE(file=trim(param_dir) // '/photolysisConstants.config', EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
     usePhotolysisConstants = .FALSE.
     WRITE (*,*) 'Photolysis constants file not found, trying photolysis rates file...'
     CALL readPhotolysisRates (ck, cl, cmm, cnn, str, tf)
     RETURN
  ENDIF
  usePhotolysisConstants = .TRUE.

  nrOfPhotoRates = 0
  WRITE (*,*) 'Reading photolysis constants from file...'
  OPEN (10, file=trim(param_dir) // '/photolysisConstants.config', status='old', iostat=ierr)
  READ (10,*)
  DO i = 1, maxNrOfPhotoRates
     READ (10,*, iostat=ierr) ck(i), cl(i), str(i)
     IF (ierr/=0) THEN
        EXIT
     ENDIF
     nrOfPhotoRates = i
  ENDDO
  CLOSE (10, status='keep')
  IF (nrOfPhotoRates>3) THEN
     WRITE (*,*) ck(1), cl(1), str(1)
     WRITE (*,*) '...'
     WRITE (*,*) ck(nrOfPhotoRates), cl(nrOfPhotoRates), str(nrOfPhotoRates)
  ELSE
     DO i = 1, nrOfPhotoRates
        WRITE (*,*) ck(i), cl(i), str(i)
     ENDDO
  ENDIF
  WRITE (*,*) 'Finished reading photolysis constants.'
  WRITE (*,*) 'Number of photolysis rates:', nrOfPhotoRates
  RETURN
END SUBROUTINE readPhotolysisConstants


SUBROUTINE getReactionListSizes (csize1, csize2)
  ! outputs csize1 and csize2, which hold the number of lines in
  ! modelConfiguration/mechanism.(reac/prod), excluding the first line and
  ! last line
  INTEGER, intent(out) :: csize1, csize2
  INTEGER :: k, l

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


SUBROUTINE getParametersFromFile (input_file, parameterArray, numValidEntries)
  ! Read in parameters from file at input_file, and save the contents of each
  ! line to an element of the array
  CHARACTER, intent(in) :: input_file*(*)
  DOUBLE PRECISION, intent(out) :: parameterArray(*)
  INTEGER, intent(out) :: numValidEntries

  OPEN (10, file=input_file, status='old') ! input file
  numValidEntries = 0
  READ (10,*) parameterArray(1)
  DO WHILE (parameterArray(numValidEntries+1)/=-9999)
     numValidEntries = numValidEntries + 1
     READ (10,*) parameterArray(numValidEntries+1)
  END DO
  CLOSE (10, status='keep')

  RETURN
END SUBROUTINE getParametersFromFile


SUBROUTINE readPhotoRates (maxNumberOfDataPoints)

  USE photolysisRates
  USE directories, ONLY : param_dir, env_constraints_dir
  USE storage, ONLY : maxPhotoRateNameLength, maxFilepathLength
  IMPLICIT NONE

  INTEGER :: i, k, ierr
  INTEGER :: maxNumberOfDataPoints
  CHARACTER (LEN=maxPhotoRateNameLength) :: string
  CHARACTER (LEN=maxFilepathLength) :: fileLocationPrefix
  CHARACTER (LEN=maxFilepathLength+maxPhotoRateNameLength) :: fileLocation

  ! GET NAMES OF PHOTO RATES
  CALL readPhotolysisConstants (ck, cl, cmm, cnn, photoRateNames, transmissionFactor)
  WRITE (*,*)

  numConPhotoRates = 0
  ! GET NAMES OF CONSTRAINED PHOTO RATES
  WRITE (*,*) 'Reading names of constrained photolysis rates from file...'
  OPEN (10, file=trim(param_dir) // '/constrainedPhotoRates.config', status='old') ! input file
  DO i = 1, maxNrOfConPhotoRates
     READ (10,*, iostat=ierr) constrainedPhotoRates(i)
     IF (ierr/=0) THEN
       EXIT
    ENDIF
    numConPhotoRates = i
  ENDDO
  CLOSE (10, status='keep')
  IF (numConPhotoRates>3) THEN
     WRITE (*,*) constrainedPhotoRates(1)
     WRITE (*,*) '...'
     WRITE (*,*) constrainedPhotoRates(numConPhotoRates)
  ELSE
     DO i = 1, numConPhotoRates
        WRITE (*,*) constrainedPhotoRates(i)
     ENDDO
  ENDIF
  WRITE (*,*) 'Finished reading names of constrained photolysis rates.'
  WRITE (*,*) 'Number of constrained photorates:', numConPhotoRates

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

  fileLocationPrefix = trim(env_constraints_dir) // "/"

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
  USE directories, ONLY : param_dir
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER (LEN=maxSpecLength) c
  CHARACTER (LEN=maxSpecLength), intent(out) :: r(*)
  INTEGER i, j, ierr
  INTEGER, intent(in) :: nsp

  WRITE (*,*) 'Reading concentration output from file...'

  i = 0

  OPEN (10, file=trim(param_dir) // '/concentrationOutput.config', status='old')
  ! Loop over all lines of the file, and add each entry to r(i)
  ! Then check we don't have more species of interest than total species
  READ (10,*, iostat=ierr) c
  DO WHILE (ierr==0)
     IF (ierr/=0) THEN
        EXIT
     ENDIF
     i = i + 1
     r(i) = c
     READ (10,*, iostat=ierr) c
  ENDDO
  CLOSE (10, status='keep')

  WRITE (*,*) 'Finished reading concentration output from file.'

  ! ERROR HANDLING
  IF (i>nsp) THEN
     WRITE (51,*) 'Error: Number of (number of species output is required for) > (number of species) '
     WRITE (51,*) "(number of species output is required for) = ", i
     WRITE (51,*) "(number of species) = ", nsp
     STOP 2
  ENDIF

  WRITE (*,*) 'Output required for concentration of', i, 'species:'
  IF (i>3) THEN
     WRITE (*,*) 1, r(1)
     WRITE (*,*) '...'
     WRITE (*,*) i, r(i)
  ELSE
     DO j = 1, i
        WRITE (*,*) j, r(j)
     ENDDO
  ENDIF

  RETURN
END SUBROUTINE readSpeciesOutputRequired


SUBROUTINE readSpecies (y, neq, speciesName, speciesNumber)
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  DOUBLE PRECISION, intent(out) :: y(*)
  INTEGER, intent(in) :: neq
  INTEGER :: j
  INTEGER, intent(out) :: speciesNumber(*)
  CHARACTER (LEN=maxSpecLength), intent(out) :: speciesName(*)

  ! Read in species number and name from mC/mechanism.species to speciesName
  ! and speciesNumber. Also set each element of y to 0.
  OPEN (10, file='modelConfiguration/mechanism.species') ! input file
  DO j = 1, neq
     READ (10,*) speciesNumber(j), speciesName(j)
     y(j) = 0
  ENDDO
  CLOSE (10, status='keep')

  RETURN
END SUBROUTINE readSpecies


SUBROUTINE readInitialConcentrations (concSpeciesName, concentration, concCounter, nsp)
  ! Reads in concentration per species from mC/initialConcentrations.config
  ! Checks that there aren't more inputs that species
  USE directories, ONLY: param_dir
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER (LEN=maxSpecLength), intent(out) :: concSpeciesName(*)
  CHARACTER (LEN=maxSpecLength) k
  DOUBLE PRECISION, intent(out) :: concentration(*)
  DOUBLE PRECISION l
  INTEGER, intent(out) :: concCounter
  INTEGER, intent(in) :: nsp
  INTEGER i

  WRITE (*,*) 'Reading initial concentrations...'

  concCounter = 0
  OPEN (10, file=trim(param_dir) // '/initialConcentrations.config', status='old') ! input file for lhs of equations
  DO
     READ (10,*) k, l
     IF (l==-1) EXIT
     concCounter = concCounter + 1
     concentration(concCounter) = l
     concSpeciesName(concCounter) = k
  ENDDO
  CLOSE (10, status='keep')

  IF (concCounter>3) THEN
     WRITE (*,*) 1, ' ', concSpeciesName(1), ' ', concentration(1)
     WRITE (*,*) '...'
     WRITE (*,*) concCounter, ' ', concSpeciesName(concCounter), ' ', concentration(concCounter)
  ELSE
     DO i = 1, concCounter
        WRITE (*,*) i, ' ', concSpeciesName(i), ' ', concentration(i)
     ENDDO
  ENDIF
  WRITE (*,*) 'Finished reading initial concentrations.'

  IF (concCounter>nsp) THEN
     WRITE (51,*) "Error:(number of species initial concentrations are set for) > (number of species) "
     WRITE (51,*) "(number of species initial concentrations are set for) = ", concCounter
     WRITE (51,*) "(number of species) = ", nsp
  ENDIF

  RETURN
END SUBROUTINE readInitialConcentrations


SUBROUTINE readProductsOfInterest (r, i)
  ! Read in contents of modelConfiguration/productionRatesOutput.config, which
  ! contains a list of the species we want to have outputted to mC/productionRates.output
  ! Output the contents in r, with i as the length of r.
  USE directories, ONLY: param_dir
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER (LEN=maxSpecLength) c
  CHARACTER (LEN=maxSpecLength), intent(out) :: r(*)
  INTEGER ierr, j
  INTEGER, intent(out) :: i
  LOGICAL file_exists

  WRITE (*,*) 'Reading products of interest...'
  INQUIRE(file=trim(param_dir) // '/productionRatesOutput.config', EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
     WRITE (*,*) 'No productionRatesOutput.config file exists, so prodIntName will be empty.'
  ELSE
     OPEN (10, file=trim(param_dir) // '/productionRatesOutput.config', status='old')
     i = 0
     READ (10, *, iostat=ierr) c
     DO WHILE (ierr==0)
        i = i + 1
        r(i) = c
        READ (10, *, iostat=ierr) c
     ENDDO
     CLOSE (10, status='keep')
  END IF
  IF (i>3) THEN
     WRITE (*,*) 1, ' ', r(1)
     WRITE (*,*) '...'
     WRITE (*,*) i, ' ', r(i)
  ELSE
     DO j = 1, i
        WRITE (*,*) j, ' ', r(j)
     ENDDO
  ENDIF
  WRITE (*,*) 'Finished reading products of interest.'
  RETURN
END SUBROUTINE readProductsOfInterest


SUBROUTINE readReactantsOfInterest (r, i)
  ! Read in contents of modelConfiguration/lossRatesOutput.config, which
  ! contains a list of the species we want to have outputted to mC/lossRates.output.
  ! Output the contents in r, with i as the length of r.
  USE directories, ONLY: param_dir
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER (LEN=maxSpecLength) c
  CHARACTER (LEN=maxSpecLength), intent(out) :: r(*)
  INTEGER ierr, j
  INTEGER, intent(out) :: i
  LOGICAL file_exists

  WRITE (*,*) 'Reading reactants of interest...'
  INQUIRE(file=trim(param_dir) // '/lossRatesOutput.config', EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
     WRITE (*,*) 'No lossRatesOutput.config file exists, so reacIntName will be empty.'
  ELSE
     OPEN (10, file=trim(param_dir) // '/lossRatesOutput.config', status='old')
     i = 0
     READ (10, *, iostat=ierr) c
     DO WHILE (ierr==0)
        i = i + 1
        r(i) = c
        READ (10, *, iostat=ierr) c
     ENDDO
     CLOSE (10, status='keep')
  END IF
  IF (i>3) THEN
     WRITE (*,*) 1, ' ', r(1)
     WRITE (*,*) '...'
     WRITE (*,*) i, ' ', r(i)
  ELSE
     DO j = 1, i
        WRITE (*,*) j, ' ', r(j)
     ENDDO
  ENDIF
  WRITE (*,*) 'Finished reading reactants of interest.'
  RETURN
END SUBROUTINE readReactantsOfInterest


SUBROUTINE readSpeciesConstraints (speciesName, neq, y, t)
  USE species
  USE constraints
  USE chemicalConstraints
  USE directories, ONLY : param_dir, spec_constraints_dir
  USE photolysisRates, ONLY : maxNrOfPhotoRates
  USE storage, ONLY : maxSpecLength, maxFilepathLength
  USE configFunctions_mod, ONLY : matchOneNameToNumber
  IMPLICIT NONE

  INTEGER :: i, j, k, dataNumberOfPoints, neq, id, ierr
  INTEGER :: countOfVarConSpecNames, countOfFixConSpecNames, countOfConNames
  CHARACTER (LEN=maxSpecLength) :: speciesName(:), name
  CHARACTER (LEN=maxFilepathLength) :: fileLocationPrefix
  CHARACTER (LEN=maxFilepathLength+maxSpecLength) :: fileLocation
  DOUBLE PRECISION :: concAtT, t, value
  DOUBLE PRECISION :: y (*)

  ! READ IN SPECIES TO BE CONSTRAINED
  WRITE (*,*) 'Counting the species to be constrained (in file constrainedSpecies.config)...'
  CALL count_lines_in_file(trim(param_dir) // '/constrainedSpecies.config', countOfVarConSpecNames)

  WRITE (*,*) 'Finished counting the names of the species to be constrained.'
  WRITE (*,*) 'Number of names for variable constrained species:', countOfVarConSpecNames

  ! read in numberOfFixedConstrainedSpecies

  WRITE (*,*) 'Counting the fixed-concentration species to be constrained (in file constrainedFixedSpecies.config)...'
  CALL count_lines_in_file(trim(param_dir) // '/constrainedFixedSpecies.config', countOfFixConSpecNames)
  WRITE (*,*) 'Finished counting the names of fixed-concentration species'

  WRITE (*,*) 'Number names of fixed constrained species:', countOfFixConSpecNames

  countOfConNames = countOfVarConSpecNames + countOfFixConSpecNames
  ALLOCATE (constrainedSpecies(countOfConNames), constrainedName(countOfConNames))



  WRITE (*,*) 'Reading in the names of variable constrained species...'
  OPEN (12, file=trim(param_dir) // '/constrainedSpecies.config', status='old') ! input file
  j = 0
  DO i = 1, maxNrOfPhotoRates
     READ (12,*, iostat=ierr) name
     IF (ierr/=0) THEN
        EXIT
     ENDIF
     CALL matchOneNameToNumber (speciesName, name, id)
     IF (id/=0) THEN
        j = j + 1
        constrainedName(j) = name
        constrainedSpecies(j) = id
     ENDIF
  END DO
  CLOSE (12, status='keep')
  numberOfVariableConstrainedSpecies = j
  WRITE (*,*) 'Finished reading the names of variable constrained species'
  WRITE (*,*) 'Number of constrained variable species:', numberOfVariableConstrainedSpecies

  WRITE (*,*) 'maxNumberOfDataPoints:', maxNumberOfDataPoints
  WRITE (*,*) 'Allocating storage for variable constrained species...'
  ALLOCATE (dataX (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
  ALLOCATE (dataY (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
  ALLOCATE (dataY2 (numberOfVariableConstrainedSpecies, maxNumberOfDataPoints))
  WRITE (*,*) 'Finished allocating storage for variable constrained species.'

  IF (numberOfVariableConstrainedSpecies>3) THEN
     WRITE (*,*) 1, constrainedName(1)
     WRITE (*,*) '...'
     WRITE (*,*) numberOfVariableConstrainedSpecies, constrainedName(numberOfVariableConstrainedSpecies)
  ELSE
     DO i = 1, numberOfVariableConstrainedSpecies
        WRITE (*,*) i, constrainedName(i)
     ENDDO
  ENDIF

  fileLocationPrefix = trim(spec_constraints_dir) // "/"

  ! READ CONCENTRATION DATA FOR VARIABLE CONSTRAINED SPECIES
  WRITE (*,*) 'Reading concentration data for constrained species...'
  ALLOCATE (speciesNumberOfPoints(numberOfVariableConstrainedSpecies+countOfFixConSpecNames))
  DO i = 1, numberOfVariableConstrainedSpecies
     IF (i<3 .OR. i==numberOfVariableConstrainedSpecies) THEN
        WRITE (*,*) constrainedName(i), '...'
     ELSE
        IF (i==2) WRITE (*,*) '...'
     ENDIF

     fileLocation = trim(fileLocationPrefix) // trim(constrainedName(i))
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
     CALL matchOneNameToNumber (speciesName, name, id)
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

  IF (numberOfFixedConstrainedSpecies>3) THEN
     WRITE (*,*) 1, constrainedName(1+numberOfVariableConstrainedSpecies), dataFixedY (1)
     WRITE (*,*) '...'
     WRITE (*,*) numberOfFixedConstrainedSpecies, &
                 constrainedName(numberOfFixedConstrainedSpecies+numberOfVariableConstrainedSpecies), &
                 dataFixedY (numberOfFixedConstrainedSpecies)
  ELSE
     DO i = 1, numberOfFixedConstrainedSpecies
        WRITE (*,*) i, constrainedName(numberOfFixedConstrainedSpecies+numberOfVariableConstrainedSpecies), &
                    dataFixedY (numberOfFixedConstrainedSpecies)
     ENDDO
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


SUBROUTINE readEnvVar ()
  ! This function reads in data from environmentVariables.config, and sets
  ! envVarTypesNum for each one. In the case of a constrained variable, this
  ! also reads in the constraint data from environmentConstraints directory, the
  ! file named after the environmental variable.
  USE envVars
  USE directories, ONLY : param_dir, env_constraints_dir
  USE constraints, ONLY : maxNumberOfDataPoints
  USE storage, ONLY : maxFilepathLength, maxEnvVarNameLength
  IMPLICIT NONE

  INTEGER :: i, counter, k
  CHARACTER (LEN=10) dummy
  CHARACTER (LEN=maxFilepathLength) :: fileLocationPrefix
  CHARACTER (LEN=maxFilepathLength+maxEnvVarNameLength) :: fileLocation

  WRITE (*,*) 'Reading environment variables...'

  ! Count number of environment variables by reading in lines from file
  CALL count_lines_in_file( trim(param_dir) // '/environmentVariables.config', counter )

  numEnvVars = counter

  ! Allocate storage for current values of env vars used for output
  ALLOCATE (currentEnvVarValues(numEnvVars))

  WRITE (*,*) 'Number of environment variables: ', numEnvVars
  ALLOCATE (envVarTypesNum(numEnvVars), envVarNames(numEnvVars), envVarTypes(numEnvVars))
  ALLOCATE (envVarFixedValues(numEnvVars))

  OPEN (10, file=trim(param_dir) // '/environmentVariables.config', status='old') ! input file
  ! Read in environment variables - if
  DO i = 1, numEnvVars
     READ (10,*) dummy, envVarNames(i), envVarTypes(i)
     WRITE (*,'(A, A4, A12, A30)') ' ', dummy, envVarNames(i), adjustr(envVarTypes(i))

     SELECT CASE (trim(envVarTypes(i)))
     CASE ('CALC')
        envVarTypesNum(i) = 1
     CASE ('CONSTRAINED')
        envVarTypesNum(i) = 2
     CASE ('NOTUSED')
        envVarTypesNum(i) = 4
     CASE default
        envVarTypesNum(i) = 3
        ! Copy 3rd column value to envVarFixedValues(i)
        READ (envVarTypes(i),*) envVarFixedValues(i)
     END SELECT
  ENDDO
  CLOSE (10, status='keep')

  WRITE (*,*) 'Finished reading environment variables.'
  WRITE (*,*)

  ! Allocate variables for next section
  ALLOCATE (envVarX (numEnvVars, maxNumberOfDataPoints))
  ALLOCATE (envVarY (numEnvVars, maxNumberOfDataPoints))
  ALLOCATE (envVarY2 (numEnvVars, maxNumberOfDataPoints))
  ALLOCATE (envVarNumberOfPoints(numEnvVars))

  ! TODO: convert this to a command line input argument
  fileLocationPrefix = trim(env_constraints_dir) // "/"
  ! If environment variable is constrained, read in constraint data
  WRITE (*,*) 'Checking for constrained environment variables...'
  DO i = 1, numEnvVars
     IF (envVarTypes(i)=='CONSTRAINED') THEN

        WRITE (*,*) 'Reading constraint data for', envVarNames(i)

        fileLocation = trim(fileLocationPrefix) // trim(envVarNames(i))

        OPEN (11, file=fileLocation, status='old')

        READ (11,*) envVarNumberOfPoints(i)
        DO k = 1, envVarNumberOfPoints(i)
           READ (11,*) envVarX (i, k), envVarY (i, k) ! envVarY2 (i, k)
        ENDDO
        CLOSE (11, status='keep')
        WRITE (*,*) 'Finished reading constraint data.'
     ENDIF
  ENDDO
  WRITE (*,*) 'Finished checking for constrained environment variables.'

  RETURN
END SUBROUTINE readEnvVar


SUBROUTINE count_lines_in_file (filename, counter)
  CHARACTER (*), intent(in) :: filename
  INTEGER, intent(out) :: counter
  CHARACTER (LEN=10) dummy
  INTEGER :: ierr

  OPEN (11, file=filename, status='old')
  counter = 0
  ierr = 0
  DO WHILE (ierr==0)
     counter = counter + 1
     READ (11, *, iostat=ierr) dummy
  ENDDO
  CLOSE (11, status='keep')
  counter = counter - 1
END SUBROUTINE count_lines_in_file
END MODULE inputFunctions_mod
