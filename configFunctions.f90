SUBROUTINE calcDateParameters ()
  USE date
  INTEGER :: i

  monthList = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
  ! calculate which day of the year day/month refers to
  totalDays = 0
  DO i = 1, month-1
     totalDays = totalDays + monthList(i)
  ENDDO
  totalDays = totalDays + day -1
  ! This day refers to the following fraction through the year
  fractionYear = totalDays / 365
  ! Set number of seconds per year
  secYear = 3.6525d+02*2.40d+01*3.60d+03
  RETURN
END SUBROUTINE calcDateParameters

SUBROUTINE writeFileHeaders (photoRateNamesForHeader)
  USE envVars
  USE photolysisRates, ONLY : nrOfPhotoRates, ck
  USE storage, ONLY : maxPhotoRateNameLength
  CHARACTER (LEN=maxPhotoRateNameLength) :: photoRateNamesForHeader(*)
  INTEGER :: i

  ! WRITE FILE OUTPUT HEADERS AND OUTPUT AT t = 0
  ! OUTPUT FOR CVODE MAIN SOLVER
  WRITE (57,*) 't LNST LNFE LNETF LNGE'

  ! OUTPUT FOR SPARSE SOLVER
  WRITE (61,*) 't NFELS NJTV NPE NPS'
  WRITE (60,*) 'time speciesNumber speciesName reactionNumber rate'
  WRITE (56,*) 'time speciesNumber speciesName reactionNumber rate'

  ! OTHER OUPUT
  ! WRITE (85,*), 't temp m h2o'
  WRITE (58,*) 't ', (trim(photoRateNamesForHeader(ck(i)) )// '    ', i = 1, nrOfPhotoRates)
  WRITE (62,*) 't currentStepSize previousStepSize'
  WRITE (59,*) 't secx cosx lat longt lha sinld cosld'
  WRITE (52,*) 'time ', (envVarNames(i), i = 1, numEnvVars), 'RO2'
  RETURN
END SUBROUTINE writeFileHeaders


SUBROUTINE matchNameToNumber (masterSpeciesList, testSpeciesList, testSpeciesListSize, &
                              masterSpeciesListSize, returnArray, returnArraySize)
  USE storage, ONLY : maxSpecLength
  ! This takes in masterSpeciesList, and checks whether each member of
  ! testspeciesList is in masterSpeciesList.
  ! When it finds a match, it adds the number of the line in masterSpeciesList to
  ! returnArray in the next available space.
  ! returnArraySize contains the size of returnArray, which is the
  ! number of times a match was made
  CHARACTER (LEN=maxSpecLength), intent(in) :: masterSpeciesList(*)
  CHARACTER (LEN=maxSpecLength), intent(inout) :: testSpeciesList(*)
  INTEGER, intent(in) :: testSpeciesListSize, masterSpeciesListSize
  INTEGER, intent(out) :: returnArray(*), returnArraySize
  INTEGER i, j
  LOGICAL match

  returnArraySize = 0
  ! loop over testSpeciesList, and masterSpeciesList. If a match is made, then append
  ! returnArray with the number of the species from testSpeciesList within the masterSpeciesList
  DO i = 1, testSpeciesListSize
     match = .FALSE.
     DO j = 1, masterSpeciesListSize
        IF (masterSpeciesList(j)==testSpeciesList(i)) THEN
           match = .TRUE.
           returnArraySize = returnArraySize + 1
           returnArray(returnArraySize) = j
        ENDIF
     ENDDO
     ! substitute empty strings for invalid species
     IF (match.eqv..FALSE.) THEN
        testSpeciesList(i) = ''
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE matchNameToNumber

SUBROUTINE matchOneNameToNumber (speciesName, oneSpecies, neq, id)
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER (LEN=maxSpecLength) oneSpecies, speciesName(*), m
  INTEGER j, neq, id

  id = 0
  DO j = 1, neq
     m = speciesName(j)
     IF (m==oneSpecies) THEN
        id = j
        RETURN
     ENDIF
  ENDDO
END SUBROUTINE matchOneNameToNumber


SUBROUTINE setConcentrations (outputConcentrations, refSpeciesNames, concSpeciesNames, inputConcentrations, concCounter, numSpecies)
  ! For each input species in concSpeciesNames, and matching value in concentrations,
  ! look through refSpeciesNames for the number of this species in that list,
  ! then transer the value from concentrations to y. If no match is found,
  ! output this to errors.output, but don't stop, just ignore the input value.
  ! Ptrint outcome of each search into initialConditionsSetting.output.
  USE storage, ONLY : maxSpecLength
  IMPLICIT NONE

  CHARACTER (LEN=maxSpecLength), intent(in) :: concSpeciesNames(*), refSpeciesNames(*)
  CHARACTER (LEN=maxSpecLength) :: k, m
  DOUBLE PRECISION, intent(in) :: inputConcentrations(*)
  DOUBLE PRECISION, intent(out) :: outputConcentrations(*)
  INTEGER, intent(in) :: concCounter, numSpecies
  INTEGER :: i, j
  LOGICAL :: match

  DO i = 1, concCounter
     match = .FALSE.
     k = concSpeciesNames(i)
     DO j = 1, numSpecies
        m = refSpeciesNames(j)
        IF (m==k) THEN
           match = .TRUE.
           ! Set concentration in y()
           outputConcentrations(j) = inputConcentrations(i)
           WRITE (54,*) 'match, m = k = ', m, ' concentration = ', inputConcentrations(i)
           EXIT
        ELSE
           WRITE (54,*) 'no match, m = ', m, ' != k = ', k, ' concentration = ', inputConcentrations(i)
        ENDIF
     ENDDO
     IF (match.eqv..FALSE.) THEN
        ! If we reach this point, we've failed to find this species
        WRITE (51,*) "Error in setConcentrations"
        WRITE (51,*) "Can't find species: ", k," in species list"
     ENDIF
  ENDDO
  RETURN
END SUBROUTINE setConcentrations
