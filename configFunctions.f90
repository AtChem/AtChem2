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
  WRITE (23,*) 't LNST LNFE LNETF LNGE'

  ! OUTPUT FOR SPARSE SOLVER
  WRITE (21,*) 't NFELS NJTV NPE NPS'
  WRITE (89,*) 'time speciesNumber speciesName reactionNumber rate'
  WRITE (90,*) 'time speciesNumber speciesName reactionNumber rate'

  ! OTHER OUPUT
  ! write(85,*), 't temp m h2o'
  WRITE (86,*) 't ', (TRIM (photoRateNamesForHeader(ck(i)) )// '    ', i = 1, nrOfPhotoRates)
  WRITE (22,*) 't currentStepSize previousStepSize'
  WRITE (49,*) 't secx cosx lat longt lha sinld cosld'
  WRITE (95,*) 'time ', (envVarNames(i), i = 1, numEnvVars), 'RO2'

  RETURN
END SUBROUTINE writeFileHeaders


SUBROUTINE matchNameToNumber (speciesName, speciesList, listSize, neq, returnArray, returnArraySize)
  CHARACTER (LEN=10) speciesList(*), speciesName(*), k, m
  INTEGER i, j, match, matched_j, neq, returnArray(*), returnArraySize, listSize
  returnArraySize = 1

  DO i = 1, listSize
     k = speciesList(i)
     match = 0
     DO j = 1, neq
        m = speciesName(j)
        IF (m==k) THEN
           match = 1
           matched_j = j
           returnArray(returnArraySize) = j
           returnArraySize = returnArraySize + 1
        ENDIF
     ENDDO
     ! substitute empty strings for invalid species
     IF (match==0) THEN
        speciesList(i) = ''
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
           WRITE (99,*) 'match, m = k = ', m, ' concentration = ', concentration(i)
        ELSE
           WRITE (99,*) 'no match, m', m, ' != k! = ', k, ' concentration = ', concentration(i)
        ENDIF
     ENDDO
     IF (match==0) THEN
        WRITE (94,*) "Error in setConcentrations"
        WRITE (94,*) "Can't find species: ", k," in species list"
     END IF
  ENDDO
  RETURN
END SUBROUTINE setConcentrations
