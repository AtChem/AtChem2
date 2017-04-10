SUBROUTINE calcDateParameters ()
  USE date
  INTEGER :: i

  monthList = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
  totalDays = 0
  DO i = 1, month-1
     totalDays = totalDays + monthList(i)
  ENDDO

  totalDays = totalDays + day -1
  fractionYear = totalDays
  fractionYear = fractionYear / 365
  secYear = 3.6525d+02*2.40d+01*3.60d+03
  RETURN
END SUBROUTINE calcDateParameters

SUBROUTINE writeFileHeaders (photoRateNamesForHeader)
  USE envVars
  USE photolysisRates, ONLY: nrOfPhotoRates, ck
  CHARACTER (LEN=30) :: photoRateNamesForHeader(*)
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


SUBROUTINE matchNameToNumber (masterSpeciesList, testSpeciesList, listSize, &
                              neq, returnArray, returnArraySize)
  CHARACTER (LEN=10), INTENT(IN) :: masterSpeciesList(*)
  CHARACTER (LEN=10), INTENT(INOUT) :: testSpeciesList(*)
  INTEGER, INTENT(IN) :: listSize, neq
  INTEGER, INTENT(OUT) :: returnArray(*), returnArraySize
  INTEGER i, j
  LOGICAL match

  returnArraySize = 1
  ! loop over testSpeciesList, and masterSpeciesList. If a match is made, then append
  ! returnArray with the number of that species within the masterSpeciesList
  DO i = 1, listSize
     match = .FALSE.
     DO j = 1, neq
        IF (masterSpeciesList(j)==testSpeciesList(i)) THEN
           match = .TRUE.
           returnArray(returnArraySize) = j
           returnArraySize = returnArraySize + 1
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
  CHARACTER (LEN=10) oneSpecies, speciesName(*), m
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
  CHARACTER (LEN=10), intent(in) :: concSpeciesNames(*), refSpeciesNames(*)
  CHARACTER (LEN=10) :: k, m
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
