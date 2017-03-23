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
  WRITE (58,*) 't ', (TRIM (photoRateNamesForHeader(ck(i)) )// '    ', i = 1, nrOfPhotoRates)
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


SUBROUTINE setConcentrations (y, speciesName, concSpeciesName, concentration, concCounter, neq)
  CHARACTER (LEN=10) concSpeciesName(*), speciesName(*), k, m
  DOUBLE PRECISION concentration(*), y(*)
  INTEGER concCounter, neq, i, j, match

  DO i = 1, concCounter
     k = concSpeciesName(i)
     ! flag for matching of string names
     match = 0
     DO j = 1, neq
        m = speciesName(j)
        IF (m==k) THEN
           ! Set concentration in y()
           y(j) = concentration(i)
           match = 1
           WRITE (54,*) 'match, m = k = ', m, ' concentration = ', concentration(i)
        ELSE
           WRITE (54,*) 'no match, m', m, ' != k! = ', k, ' concentration = ', concentration(i)
        ENDIF
     ENDDO
     IF (match==0) THEN
        WRITE (51,*) "Error in setConcentrations"
        WRITE (51,*) "Can't find species: ", k," in species list"
     END IF
  ENDDO
  RETURN
END SUBROUTINE setConcentrations
